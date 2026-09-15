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
