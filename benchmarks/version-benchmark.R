## Benchmark one installed stagedtrees version, for one repetition.
##
## Usage:  Rscript version-benchmark.R <lib-dir> <label> <out.csv> <rep>
##
## <lib-dir> is a library directory into which the version under test has
## already been installed with R CMD INSTALL. It is NOT a source tree, and
## pkgload::load_all() must not be used here: load_all compiles with the
## debug flags (-O0), while R CMD INSTALL uses -O2, and the difference on
## this package's compiled kernels is about 3.9x. Benchmarking a load_all
## tree measures unoptimised object code and understates every version that
## carries src/.
##
## Performs ONE repetition of each workload and appends to <out.csv>. The
## caller is expected to vary the version in the inner loop and the
## repetition in the outer loop, so that the versions are interleaved in
## time -- see run-versions.sh.
##
## This matters. An earlier version of this script took a repetition count
## and ran every repetition of one version before moving to the next. Any
## drift in machine performance over the run (thermal, noisy neighbours,
## page cache) is then confounded with the version, and appears as a
## spurious version effect. That design produced an apparent 13.7% slowdown
## in predict() between two versions whose relevant code paths are
## identical; interleaved measurement put the difference at -2.0% with a
## 95% confidence interval straddling zero. Keep the versions interleaved.

args <- commandArgs(trailingOnly = TRUE)
lib <- args[[1]]
label <- args[[2]]
out <- args[[3]]
rep_id <- if (length(args) >= 4) as.integer(args[[4]]) else 1L
reps <- 1L

.libPaths(c(lib, .libPaths()))
suppressMessages(library(stagedtrees))
## fail loudly rather than silently benchmarking whatever else is installed
stopifnot(identical(normalizePath(dirname(getNamespaceInfo("stagedtrees", "path"))),
                    normalizePath(lib)))

mkdata <- function(n, p, lv, seed = 1) {
  set.seed(seed)
  as.data.frame(
    lapply(seq_len(p), function(i) factor(sample(letters[seq_len(lv)], n, replace = TRUE))),
    col.names = paste0("V", seq_len(p))
  )
}

## Workloads sweep the MODEL DIMENSION rather than fixing one size per
## function. A single arbitrary size per function is how an earlier version
## of this file ranked sample_from as the dominant cost: it was handed 20000
## samples on an 8-variable model while stages_bhc got a 5-variable one. On a
## common model the order inverts, because the functions scale in different
## variables entirely -- stages_bhc in situations squared (so lv^(2(p-1))),
## sample_from linearly in the number of samples drawn. Sweeping p makes that
## visible instead of letting workload choice decide the answer.
##
## p = 7 is deliberately absent: stages_bhc does not complete there in
## reasonable time in v0 or v1, which is itself a reported result. It is
## feasible in v2 (about 8 s), but including a size only one version can run
## would leave nothing to compare it against, so the p=7 figure is reported
## separately rather than through this sweep.

PS <- c(4L, 5L, 6L)          # variables
NOBS <- 2000L                # observations
LV <- 3L                     # levels per variable
NSAMP <- 2000L               # draws for sample_from
NROW <- 1000L                # rows for prob / predict

workloads <- list()
add <- function(name, size, setup, run) {
  workloads[[length(workloads) + 1L]] <<-
    list(name = name, size = size, setup = setup, run = run)
}

for (p in PS) {
  local({
    p <- p
    dim <- sprintf("%dobs x %dvar x %dlv", NOBS, p, LV)
    add("full", dim,
        function() mkdata(NOBS, p, LV),
        function(d) full(d, lambda = 1))
    add("sevt_fit", dim,
        function() full(mkdata(NOBS, p, LV), lambda = 1, join_unobserved = FALSE),
        function(m) sevt_fit(m))
    add("stages_bhc", dim,
        function() full(mkdata(NOBS, p, LV), lambda = 1),
        function(m) stages_bhc(m))
    ## the DEFAULT searches over k, refitting per candidate; it costs ~30x the
    ## fixed-k call and is what users actually invoke
    add("stages_hclust", paste(dim, "(default)"),
        function() full(mkdata(NOBS, p, LV), lambda = 1),
        function(m) stages_hclust(m))
    add("sample_from", paste(dim, sprintf("(n=%d)", NSAMP)),
        function() full(mkdata(NOBS, p, LV), lambda = 1),
        function(m) sample_from(m, NSAMP))
    add("prob", paste(dim, sprintf("(%d rows)", NROW)),
        function() {
          d <- mkdata(NOBS, p, LV)
          list(m = full(d, lambda = 1), d = d[seq_len(NROW), seq_len(min(4L, p))])
        },
        function(x) prob(x$m, x$d))
    add("predict", paste(dim, sprintf("(%d rows)", NROW)),
        function() {
          d <- mkdata(NOBS, p, LV)
          list(m = full(d, lambda = 1), d = d[seq_len(NROW), ])
        },
        function(x) predict(x$m, x$d))
  })
}

rows <- list()
for (w in workloads) {
  x <- try(w$setup(), silent = TRUE)
  if (inherits(x, "try-error")) {
    message(sprintf("  %-14s %-12s SETUP FAILED", w$name, w$size))
    next
  }
  ok <- TRUE
  ts <- numeric(reps)
  for (r in seq_len(reps)) {
    invisible(gc(verbose = FALSE))
    tt <- try(system.time(invisible(w$run(x)))[["elapsed"]], silent = TRUE)
    if (inherits(tt, "try-error")) { ok <- FALSE; break }
    ts[r] <- tt
  }
  if (!ok) {
    message(sprintf("  %-14s %-12s RUN FAILED", w$name, w$size))
    next
  }
  rows[[length(rows) + 1L]] <- data.frame(
    version = label, workload = w$name, size = w$size,
    rep = rep_id, seconds = ts, stringsAsFactors = FALSE
  )
  message(sprintf("  %-14s %-12s %8.3fs", w$name, w$size, ts[1]))
}

res <- do.call(rbind, rows)
## append, so the caller can interleave versions across repetitions
if (file.exists(out)) {
  write.table(res, out, sep = ",", row.names = FALSE, col.names = FALSE,
              append = TRUE, qmethod = "double")
} else {
  write.csv(res, out, row.names = FALSE)
}
