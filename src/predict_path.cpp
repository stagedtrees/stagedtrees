#include <Rcpp.h>
using namespace Rcpp;

// Class-conditional log-probabilities for fully observed rows.
//
// codes    : n x p integer level codes (1-based), tree order. The class column
//            is ignored: it is overwritten by each candidate class level.
// ls       : levels per variable
// stagemap : per variable, situation index -> stage row of probs[[j]]
// probs    : per variable, nstage x nlevel probability matrix (may hold NA)
// class_pos: 1-based position of the class variable
//
// The situation index is carried along the walk, exactly as path_probability
// does: idx_j = (idx_{j-1} - 1) * ls_j + m_j, and the stage is looked up with
// the same ((idx - 1) %% nstage) + 1 wrap that find_stage applies.
//
// Two R behaviours are reproduced deliberately rather than improved on:
//   * res[is.nan(res)] <- -Inf converts NaN but NOT NA, so an unobserved stage
//     (NA probabilities) leaves NA in the output and it propagates.
//   * normalisation is res - log(sum(exp(res))), computed naively with no
//     max-subtraction. A stabilised form would differ in the last bits and in
//     the all -Inf case, which R turns into NaN.
// [[Rcpp::export]]
NumericMatrix predict_lp_cpp(IntegerMatrix codes, IntegerVector ls,
                             List stagemap, List probs, int class_pos) {
  int n = codes.nrow(), p = codes.ncol();
  if (class_pos < 1 || class_pos > p) stop("class_pos out of range");
  if (ls.size() != p || stagemap.size() != p || probs.size() != p)
    stop("ls, stagemap and probs must have one entry per variable");
  int cpos = class_pos - 1;
  int kc = ls[cpos];
  NumericMatrix out(n, kc);

  std::vector<IntegerVector> sm(p);
  std::vector<NumericMatrix> pr(p);
  for (int j = 0; j < p; j++) {
    sm[j] = as<IntegerVector>(stagemap[j]);
    pr[j] = as<NumericMatrix>(probs[j]);
    if (pr[j].ncol() != ls[j])
      stop("probability matrix for variable %d has the wrong number of levels",
           j + 1);
  }

  // Validate the level codes before walking anything. The caller only sends
  // rows with no missing predictor, but an out-of-range code here indexes the
  // probability matrices out of bounds, which is a segfault rather than a
  // wrong answer -- so it is checked once, up front, at O(n p).
  for (int j = 0; j < p; j++) {
    if (j == cpos) continue;
    for (int i = 0; i < n; i++) {
      int lev = codes(i, j);
      if (IntegerMatrix::is_na(lev) || lev < 1 || lev > ls[j])
        stop("level code out of range for variable %d", j + 1);
    }
  }
  for (int j = 0; j < p; j++) {
    for (int t = 0; t < sm[j].size(); t++) {
      int st = sm[j][t];
      if (IntegerVector::is_na(st) || st < 1 || st > pr[j].nrow())
        stop("stage index out of range for variable %d", j + 1);
    }
  }

  for (int i = 0; i < n; i++) {
    for (int c = 1; c <= kc; c++) {
      double lp = 0.0;
      long idx = 0;
      bool na = false, nan = false;
      for (int j = 0; j < p; j++) {
        int lev = (j == cpos) ? c : codes(i, j);
        int stage;
        if (j == 0) {
          stage = 1;
        } else {
          long m = sm[j].size();
          stage = sm[j][((idx - 1) % m + m) % m];
        }
        double pv = pr[j](stage - 1, lev - 1);
        if (ISNA(pv)) { na = true; }
        else if (ISNAN(pv)) { nan = true; }
        else { lp += std::log(pv); }
        idx = (j == 0) ? lev : (idx - 1) * ls[j] + lev;
      }
      if (na) out(i, c - 1) = NA_REAL;
      else if (nan) out(i, c - 1) = R_NaN;
      else out(i, c - 1) = lp;
    }
    // res[is.nan(res)] <- -Inf, then res - log(sum(exp(res)))
    // R's sum() accumulates in long double; matching it is what makes the
    // normalisation bitwise equal rather than merely equal to 1e-12
    long double s = 0.0L;
    for (int c = 0; c < kc; c++) {
      double v = out(i, c);
      if (ISNAN(v) && !ISNA(v)) { v = R_NegInf; out(i, c) = v; }
      s += (long double) std::exp(v);
    }
    double ls_ = std::log((double) s);
    for (int c = 0; c < kc; c++) out(i, c) = out(i, c) - ls_;
  }
  return out;
}
