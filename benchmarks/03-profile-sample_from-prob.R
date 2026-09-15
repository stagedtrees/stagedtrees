pkgload::load_all(".", quiet=TRUE); set.seed(1)
mk <- function(n,p,lv=3) as.data.frame(lapply(seq_len(p), function(i)
  factor(sample(letters[1:lv],n,replace=TRUE))), col.names=paste0("V",seq_len(p)))
big <- mk(20000,8,4); mb <- full(big, lambda=1)
f <- "benchmarks/sf.out"
Rprof(f, interval=0.005); invisible(sample_from(mb, 20000)); Rprof(NULL)
cat("=== sample_from ===\n"); print(head(summaryRprof(f)$by.self, 10))
Rprof(f, interval=0.005); invisible(prob(mb, big[1:5000,1:4])); Rprof(NULL)
cat("\n=== prob ===\n"); print(head(summaryRprof(f)$by.self, 10))
