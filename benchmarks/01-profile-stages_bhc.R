pkgload::load_all(".", quiet = TRUE)
set.seed(42)
mk <- function(n = 2000, p = 6, lv = 3) {
  as.data.frame(lapply(seq_len(p), function(i)
    factor(sample(letters[1:lv], n, replace = TRUE))),
    col.names = paste0("V", seq_len(p)))
}
DD <- mk()
m0 <- full(DD, lambda = 1)
cat("situations:", sum(sapply(stages(m0), length)), "\n")

Rprof("benchmarks/bhc.out", interval = 0.005, line.profiling = TRUE)
invisible(stages_bhc(m0))
Rprof(NULL)
s <- summaryRprof("benchmarks/bhc.out")
cat("\n=== TOTAL", s$sampling.time, "s ===\n")
cat("\n--- self time top 20 ---\n"); print(head(s$by.self, 20))
