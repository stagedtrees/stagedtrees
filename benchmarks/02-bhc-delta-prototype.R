pkgload::load_all(".", quiet = TRUE)
set.seed(42)
mk <- function(n=2000,p=6,lv=3) as.data.frame(lapply(seq_len(p), function(i)
  factor(sample(letters[1:lv], n, replace=TRUE))), col.names=paste0("V",seq_len(p)))
DD <- mk(p=5); m0 <- full(DD, lambda=1)

## BHC rewritten to score candidates from scalars; joins only the winning pair.
bhc_fast <- function(object, ignore = object$name_unobserved) {
  nobs <- attr(object$ll, "nobs")
  for (v in sevt_varnames(object)[-1]) {
    repeat {
      pv <- object$prob[[v]]; k <- length(object$tree[[v]]); lam <- object$lambda
      if (is.null(lam)) lam <- 0
      stg <- unique(object$stages[[v]]); stg <- stg[!(stg %in% ignore)]
      if (length(stg) < 2) break
      best <- 0; bi <- NA; bj <- NA
      ddf <- -(k - 1)
      pen <- ddf * log(nobs)          # BIC delta = -2*dll + ddf*log(n); score = -BIC
      for (i in seq_along(stg)) for (j in seq_len(i-1)) {
        p1 <- pv[[stg[i]]]; p2 <- pv[[stg[j]]]
        n1 <- attr(p1,"n"); n2 <- attr(p2,"n")
        if (is.null(n1)||is.na(n1)) n1 <- 1
        if (is.null(n2)||is.na(n2)) n2 <- 1
        c1 <- p1; c1[is.na(c1)] <- 0; ct1 <- c1*(n1+lam*k) - lam
        c2 <- p2; c2[is.na(c2)] <- 0; ct2 <- c2*(n2+lam*k) - lam
        dll <- sum(ct2[ct2>0]*log(p2[ct2>0])) + sum(ct1[ct1>0]*log(p1[ct1>0]))
        np <- ct2+ct1+lam; np <- np/sum(np); ctn <- ct1+ct2
        d <- -dll + sum(ctn[ctn>0]*log(np[ctn>0]))
        dscore <- 2*d - pen            # change in score(=-BIC)
        if (dscore >= best) { best <- dscore; bi <- stg[i]; bj <- stg[j] }
      }
      if (is.na(bi)) break
      object <- join_stages_unsafe(object, v, bi, bj)
    }
  }
  object
}

cat("--- correctness: same stage structure as stages_bhc? ---\n")
t1 <- system.time(a <- stages_bhc(m0))[["elapsed"]]; t2 <- system.time(b <- bhc_fast(m0))[["elapsed"]]
cat("stages identical:", identical(lapply(stages(a),as.character),
                                   lapply(stages(b),as.character)), "\n")
cat("logLik equal:", isTRUE(all.equal(as.numeric(logLik(a)), as.numeric(logLik(b)))),
    " df equal:", attr(logLik(a),"df")==attr(logLik(b),"df"), "\n\n")
cat("--- end-to-end timing ---\n")
cat(sprintf("stages_bhc (current): %7.2fs\nbhc_fast  (delta)   : %7.2fs\nspeedup: %.1fx\n", t1,t2,t1/t2))
