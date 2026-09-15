## Benchmark one stagedtrees source tree, for one repetition.
##
## Usage:  Rscript version-benchmark.R <pkg-source> <label> <out.csv> <rep>
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
pkg <- args[[1]]
label <- args[[2]]
out <- args[[3]]
rep_id <- if (length(args) >= 4) as.integer(args[[4]]) else 1L
reps <- 1L

suppressMessages(pkgload::load_all(pkg, quiet = TRUE))

mkdata <- function(n, p, lv, seed = 1) {
  set.seed(seed)
  as.data.frame(
    lapply(seq_len(p), function(i) factor(sample(letters[seq_len(lv)], n, replace = TRUE))),
    col.names = paste0("V", seq_len(p))
  )
}

## each entry: setup() is untimed, run(x) is timed
workloads <- list(
  list(name = "full", size = "5000x6x3",
       setup = function() mkdata(5000, 6, 3),
       run = function(d) full(d, lambda = 1)),
  list(name = "full", size = "20000x8x4",
       setup = function() mkdata(20000, 8, 4),
       run = function(d) full(d, lambda = 1)),
  list(name = "full_nojoin", size = "20000x8x4",
       setup = function() mkdata(20000, 8, 4),
       run = function(d) full(d, lambda = 1, join_unobserved = FALSE)),
  list(name = "sevt_fit", size = "20000x8x4",
       setup = function() full(mkdata(20000, 8, 4), lambda = 1, join_unobserved = FALSE),
       run = function(m) sevt_fit(m)),
  list(name = "stages_bhc", size = "2000x5x3",
       setup = function() full(mkdata(2000, 5, 3), lambda = 1),
       run = function(m) stages_bhc(m)),
  list(name = "stages_hclust", size = "2000x6x3",
       setup = function() full(mkdata(2000, 6, 3), lambda = 1),
       run = function(m) stages_hclust(m, k = 3)),
  list(name = "sample_from", size = "20000x8x4",
       setup = function() full(mkdata(20000, 8, 4), lambda = 1),
       run = function(m) sample_from(m, 20000)),
  list(name = "predict", size = "2000rows",
       setup = function() {
         d <- mkdata(5000, 6, 3)
         list(m = full(d, lambda = 1), d = d[seq_len(2000), ])
       },
       run = function(x) predict(x$m, x$d)),
  list(name = "prob", size = "5000rows",
       setup = function() {
         d <- mkdata(20000, 8, 4)
         list(m = full(d, lambda = 1), d = d[seq_len(5000), 1:4])
       },
       run = function(x) prob(x$m, x$d))
)

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
