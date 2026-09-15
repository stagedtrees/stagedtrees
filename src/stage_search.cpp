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
// Counts are reconstructed from probabilities exactly as join_ll_delta() does
// in R, since that function is the oracle the tests check this against.
//
// The deltas are accumulated as a SINGLE sum of per-level differences rather
// than as (sum of new) - (sum of old). Those two are algebraically identical
// but not numerically: the second subtracts two large, nearly equal
// log-likelihoods, so when the true delta is zero -- two stages with identical
// probability vectors, which balanced data produces readily -- the result is
// pure rounding residue. Measured, that form disagreed with an extended
// precision reference on 1 of 72 sweeps; the fused form below disagrees on
// none, while running about four times faster than accumulating in long
// double. Do not "simplify" it back into two sums.
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
      double s = 0.0;
      for (int l = 0; l < k; l++) {
        double p1 = pm(i, l), p2 = pm(j, l);
        double c1 = NumericMatrix::is_na(p1) ? 0.0 : p1;
        double c2 = NumericMatrix::is_na(p2) ? 0.0 : p2;
        ct1[l] = c1 * (n1 + lambda * k) - lambda;
        ct2[l] = c2 * (n2 + lambda * k) - lambda;
        np[l] = ct1[l] + ct2[l] + lambda;
        s += np[l];
      }
      double v = 0.0;                       // fused: one sum, no cancellation
      for (int l = 0; l < k; l++) {
        double c = ct1[l] + ct2[l];
        if (c > 0) v += c * std::log(np[l] / s);
        if (ct2[l] > 0) v -= ct2[l] * std::log(pm(j, l));
        if (ct1[l] > 0) v -= ct1[l] * std::log(pm(i, l));
      }
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
  NumericMatrix tot(nstage, k);
  std::vector<int> cnt(nstage, 0);
  for (int s = 0; s < nsit; s++) {
    int a = asg[s];
    if (a < 0) continue;
    cnt[a]++;
    for (int l = 0; l < k; l++) tot(a, l) += ct(s, l);
  }
  std::vector<double> den(nstage);
  for (int a = 0; a < nstage; a++) {
    double n = 0.0;
    for (int l = 0; l < k; l++) n += tot(a, l);
    den[a] = n + lambda * k;
  }
  // change in one stage's log-likelihood when its counts go from old to new,
  // summed per level so the two large log-likelihoods never appear separately
  auto delta_stage = [&](const double *co, double dold,
                         const double *cn, int len) {
    double dn = 0.0;
    for (int l = 0; l < len; l++) dn += cn[l];
    dn += lambda * len;
    double acc = 0.0;
    for (int l = 0; l < len; l++) {
      if (cn[l] > 0) acc += cn[l] * std::log((cn[l] + lambda) / dn);
      if (co[l] > 0) acc -= co[l] * std::log((co[l] + lambda) / dold);
    }
    return acc;
  };
  double bestv[3] = {R_NegInf, R_NegInf, R_NegInf};
  int bs[3] = {-1, -1, -1}, bt[3] = {-1, -1, -1};
  std::vector<double> srcOld(k), srcNew(k), dstOld(k), dstNew(k), empty(k, 0.0);
  for (int s = 0; s < nsit; s++) {
    int a = asg[s];
    if (a < 0) continue;
    bool singleton = (cnt[a] == 1);
    for (int l = 0; l < k; l++) {
      srcOld[l] = tot(a, l);
      srcNew[l] = tot(a, l) - ct(s, l);
    }
    double d_src = singleton
      ? delta_stage(&srcOld[0], den[a], &empty[0], k)
      : delta_stage(&srcOld[0], den[a], &srcNew[0], k);
    for (int b = 0; b < nstage; b++) {
      if (b == a) continue;
      for (int l = 0; l < k; l++) {
        dstOld[l] = tot(b, l);
        dstNew[l] = tot(b, l) + ct(s, l);
      }
      double v = d_src + delta_stage(&dstOld[0], den[b], &dstNew[0], k);
      int g = singleton ? 0 : 1;
      if (v >= bestv[g]) { bestv[g] = v; bs[g] = s; bt[g] = b; }
    }
    if (!singleton) {
      for (int l = 0; l < k; l++) dstNew[l] = ct(s, l);
      double v = d_src + delta_stage(&empty[0], 1.0, &dstNew[0], k);
      if (v >= bestv[2]) { bestv[2] = v; bs[2] = s; bt[2] = -2; }
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
