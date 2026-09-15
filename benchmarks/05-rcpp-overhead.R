library(Rcpp); set.seed(1)
sourceCpp(code = '
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double rcpp_noop() { return 0.0; }
// single path -> index (per-call, like current design)
// [[Rcpp::export]]
int tree_idx_cpp(IntegerVector lvl, IntegerVector ls) {
  int k = lvl.size(); long idx = 0, stride = 1;
  for (int i = k-1; i >= 0; --i) { idx += (long)(lvl[i]-1)*stride; stride *= ls[i]; }
  return (int)idx + 1;
}
// WHOLE sampling loop in C: n samples x p vars, one .Call
// [[Rcpp::export]]
IntegerMatrix sample_paths_cpp(int n, IntegerVector ls, NumericMatrix probs, IntegerMatrix stage_of) {
  int p = ls.size(); IntegerMatrix out(n, p);
  for (int s = 0; s < n; ++s) {
    long idx = 0;
    for (int j = 0; j < p; ++j) {
      int st = (j==0) ? 0 : stage_of(j, idx);
      double u = R::unif_rand(), c = 0.0; int pick = ls[j]-1;
      for (int l = 0; l < ls[j]; ++l) { c += probs(st, l); if (u <= c) { pick = l; break; } }
      out(s, j) = pick + 1;
      idx = idx * ls[j] + pick;
    }
  }
  return out;
}')
lvl <- c(2L,3L,1L,4L,2L,1L,3L); ls <- rep(4L,8)
N <- 20000
b <- function(l,f){f();t<-system.time(for(i in seq_len(N))f())[["elapsed"]]
  cat(sprintf("%-40s %8.2f us/call\n",l,t/N*1e6)); t/N*1e6}
cat("=== per-call .Call overhead floor ===\n")
o <- b("Rcpp no-op function", function() rcpp_noop())
c1<- b("tree_idx_cpp (per-call)", function() tree_idx_cpp(lvl, ls))
cat(sprintf("\n-> .Call overhead is %.2f us; real work is only %.2f us\n", o, c1-o))
cat("\n=== whole-loop in C: 20000 samples x 8 vars, ONE .Call ===\n")
probs <- matrix(0.25, nrow=64, ncol=4); stage_of <- matrix(0L, nrow=8, ncol=70000)
t <- system.time(sample_paths_cpp(20000L, ls, probs, stage_of))[["elapsed"]]
cat(sprintf("%-40s %8.3f s total (%.2f us/sample)\n", "sample_paths_cpp", t, t/20000*1e6))
