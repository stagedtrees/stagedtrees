// Kernels for the stage-structure searches.
//
// Design note. These compute, for every candidate move, the change in
// log-likelihood (dll) and the change in degrees of freedom (ddf), and return
// the best candidate by dll *within each distinct ddf value*. They never
// evaluate the score.
//
// That split is what lets an arbitrary R score function be used without
// costing anything asymptotically. Candidates sharing a ddf differ only in
// their log-likelihood, so any score increasing in log-likelihood ranks them
// exactly as dll does; the score is therefore needed only to choose among the
// few group representatives and to accept or reject, which R does. A callback
// into R per candidate would cost microseconds each and erase the gain.
//
// The arithmetic below deliberately mirrors join_ll_delta() in R
// element-for-element, including reconstructing counts from probabilities
// rather than taking counts directly. join_ll_delta is the oracle the tests
// check this against; computing the same quantity a different way would
// invite float-level divergence in exactly the comparisons that decide which
// model is selected.
//
// Ties resolve to the LAST candidate in iteration order, matching `>=` in the
// R implementations. Not cosmetic: on balanced data every candidate can carry
// an identical dll, and `>` versus `>=` then selects a different model.

#include <Rcpp.h>
using namespace Rcpp;

//' Best pairwise stage merge, by log-likelihood
//'
//' @param pm stage-by-level matrix of probabilities.
//' @param nvec per-stage sample sizes; non-finite entries are treated as 1,
//'   as \code{join_ll_delta} does.
//' @param lambda smoothing parameter.
//' @param k number of levels of the variable.
//' @return numeric vector of length three: the 1-based indices of the two
//'   stages to merge, and the resulting change in log-likelihood. Every
//'   pairwise merge changes the degrees of freedom by the same amount, so a
//'   single representative is returned.
//' @keywords internal
// [[Rcpp::export]]
NumericVector best_merge_cpp(NumericMatrix pm, NumericVector nvec,
                             double lambda, int k) {
  int d = pm.nrow();
  double best = R_NegInf;
  int bi = -1, bj = -1;
  std::vector<double> ct1(k), ct2(k), np(k);
  for (int i = 1; i < d; i++) {
    for (int j = 0; j < i; j++) {
      double n1 = nvec[i], n2 = nvec[j];
      if (!R_finite(n1)) n1 = 1.0;
      if (!R_finite(n2)) n2 = 1.0;
      for (int l = 0; l < k; l++) {
        double p1 = pm(i, l), p2 = pm(j, l);
        double c1 = NumericMatrix::is_na(p1) ? 0.0 : p1;
        double c2 = NumericMatrix::is_na(p2) ? 0.0 : p2;
        ct1[l] = c1 * (n1 + lambda * k) - lambda;
        ct2[l] = c2 * (n2 + lambda * k) - lambda;
      }
      // R evaluates sum(ct2 * log(p2)) and sum(ct1 * log(p1)) as two separate
      // reductions and then adds them, accumulating each in long double as
      // R's sum() does. Interleaving them, or accumulating in double, changes
      // the last bits -- which decides the comparison below on near-ties.
      long double s2 = 0.0L, s1 = 0.0L;
      for (int l = 0; l < k; l++)
        if (ct2[l] > 0) s2 += (long double)(ct2[l] * std::log(pm(j, l)));
      for (int l = 0; l < k; l++)
        if (ct1[l] > 0) s1 += (long double)(ct1[l] * std::log(pm(i, l)));
      long double dll = s2 + s1;
      long double sn = 0.0L;
      for (int l = 0; l < k; l++) { np[l] = ct2[l] + ct1[l] + lambda; sn += (long double)np[l]; }
      double snd = (double)sn;
      long double acc = 0.0L;
      for (int l = 0; l < k; l++) {
        double ctn = ct1[l] + ct2[l];
        if (ctn > 0) acc += (long double)(ctn * std::log(np[l] / snd));
      }
      double v = (double)(-dll + acc);
      if (v >= best) { best = v; bi = i; bj = j; }
    }
  }
  return NumericVector::create(bi + 1, bj + 1, best);
}

// Best single-situation move, grouped by change in degrees of freedom.
//
// stages_hc moves one situation from its stage to another (or to a brand-new
// stage). Unlike a merge, the df change is NOT constant across candidates:
//
//   to an existing stage, source not a singleton     ddf =  0
//   to a new stage                                   ddf = +(k-1)
//   away from a singleton, dissolving the source     ddf = -(k-1)
//   both at once (singleton -> new stage) is a no-op and is skipped
//
// So log-likelihood alone cannot rank all candidates: a move to a new stage
// always fits better and would always win. Candidates are therefore grouped
// by ddf and the best by log-likelihood is returned WITHIN each group, which
// R then scores -- at most three score evaluations instead of one per
// candidate. Within a group df is constant, so the same argument as for
// merges applies and any score increasing in log-likelihood agrees.
//
// ct is situations x levels of counts; asg maps situation -> 0-based stage.
// Returns a 4-column matrix, one row per ddf group:
//   situation (1-based), target stage (1-based, or 0 meaning "a new stage"),
//   dll, ddf
//
// [[Rcpp::export]]
NumericMatrix best_move_cpp(NumericMatrix ct, IntegerVector asg,
                            int nstage, double lambda) {
  int nsit = ct.nrow(), k = ct.ncol();
  // per-stage totals and sizes
  NumericMatrix tot(nstage, k);
  std::vector<int> cnt(nstage, 0);
  for (int s = 0; s < nsit; s++) {
    int a = asg[s];
    if (a < 0) continue;
    cnt[a]++;
    for (int l = 0; l < k; l++) tot(a, l) += ct(s, l);
  }
  // log-likelihood of a stage from its count row, matching R's sum() in
  // long double
  auto ll_of = [&](const double *c) {
    long double n = 0.0L;
    for (int l = 0; l < k; l++) n += (long double)c[l];
    long double denom = n + (long double)(lambda * k);
    if (denom <= 0) return (long double)0.0L;
    long double acc = 0.0L;
    for (int l = 0; l < k; l++)
      if (c[l] > 0) acc += (long double)c[l] * std::log((long double)((c[l] + lambda)) / denom);
    return acc;
  };
  std::vector<long double> base(nstage);
  std::vector<double> row(k);
  for (int a = 0; a < nstage; a++) {
    for (int l = 0; l < k; l++) row[l] = tot(a, l);
    base[a] = ll_of(&row[0]);
  }
  // three groups: ddf = -(k-1), 0, +(k-1)
  double bestv[3] = {R_NegInf, R_NegInf, R_NegInf};
  int bs[3] = {-1, -1, -1}, bt[3] = {-1, -1, -1};
  std::vector<double> src(k), dst(k);
  for (int s = 0; s < nsit; s++) {
    int a = asg[s];
    if (a < 0) continue;
    bool singleton = (cnt[a] == 1);
    for (int l = 0; l < k; l++) src[l] = tot(a, l) - ct(s, l);
    long double ll_src_new = singleton ? (long double)0.0L : ll_of(&src[0]);
    // move to each existing stage
    for (int b = 0; b < nstage; b++) {
      if (b == a) continue;
      for (int l = 0; l < k; l++) dst[l] = tot(b, l) + ct(s, l);
      long double dll = (ll_src_new + ll_of(&dst[0])) - (base[a] + base[b]);
      int g = singleton ? 0 : 1;              // dissolving source, or neither
      double v = (double)dll;
      if (v >= bestv[g]) { bestv[g] = v; bs[g] = s; bt[g] = b; }
    }
    // move to a brand-new stage; pointless if the source is a singleton,
    // since that just renames the stage
    if (!singleton) {
      for (int l = 0; l < k; l++) dst[l] = ct(s, l);
      long double dll = (ll_src_new + ll_of(&dst[0])) - base[a];
      double v = (double)dll;
      if (v >= bestv[2]) { bestv[2] = v; bs[2] = s; bt[2] = -2; }  // -2 -> new
    }
  }
  int nrow = 0;
  for (int g = 0; g < 3; g++) if (bs[g] >= 0) nrow++;
  NumericMatrix out(nrow, 4);
  int r = 0;
  double ddf[3] = {-(double)(k - 1), 0.0, (double)(k - 1)};
  for (int g = 0; g < 3; g++) {
    if (bs[g] < 0) continue;
    out(r, 0) = bs[g] + 1;
    out(r, 1) = (bt[g] == -2) ? 0 : (bt[g] + 1);
    out(r, 2) = bestv[g];
    out(r, 3) = ddf[g];
    r++;
  }
  return out;
}
