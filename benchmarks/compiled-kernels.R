## Measure one search-function/size cell, for one version, and append it.
##
## Usage: Rscript compiled-kernels.R <lib-dir> <label> <fun> <p> <out.csv>
##
## One cell per process, so that a cell which does not finish can be killed
## from the outside (see run-compiled.sh) without losing the cells already
## measured. The pure-R versions of these searches do not complete at the
## larger sizes, which is the point of the table; an in-process time limit
## would have to interrupt R code mid-search and is less reliable than
## letting the caller use timeout(1).
##
## <lib-dir> must be a library the version was INSTALLED into, not a source
## tree loaded with pkgload::load_all(): load_all compiles at -O0 and
## R CMD INSTALL at -O2, a 3.9x difference on the compiled kernels.

args <- commandArgs(trailingOnly = TRUE)
lib <- args[[1]]; label <- args[[2]]; fun <- args[[3]]
p <- as.integer(args[[4]]); out <- args[[5]]

.libPaths(c(lib, .libPaths()))
suppressMessages(library(stagedtrees))
stopifnot(identical(normalizePath(dirname(getNamespaceInfo("stagedtrees", "path"))),
                    normalizePath(lib)))

NOBS <- 5000L
LV <- 3L

set.seed(1)
d <- as.data.frame(
  lapply(seq_len(p), function(i) factor(sample(letters[seq_len(LV)], NOBS, replace = TRUE))),
  col.names = paste0("V", seq_len(p)))
m <- full(d, lambda = 1)

invisible(gc(verbose = FALSE))
el <- system.time(invisible(match.fun(fun)(m)))[["elapsed"]]

res <- data.frame(version = label, fun = fun, p = p, nobs = NOBS, lv = LV,
                  seconds = el, stringsAsFactors = FALSE)
if (file.exists(out)) {
  write.table(res, out, sep = ",", row.names = FALSE, col.names = FALSE,
              append = TRUE, qmethod = "double")
} else {
  write.csv(res, out, row.names = FALSE)
}
message(sprintf("  %-12s %-11s p=%d  %8.3fs", label, fun, p, el))
