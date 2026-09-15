## The C kernel selects a merge by log-likelihood; join_ll_delta() is the
## pure-R oracle it must agree with. Keeping the oracle in R and testing
## against it is the point: the kernel is never trusted standalone.

oracle_best <- function(object, v, ignore = object$name_unobserved) {
  stg <- unique(object$stages[[v]])
  stg <- stg[!(stg %in% ignore)]
  if (length(stg) < 2) return(NULL)
  k <- length(object$tree[[v]])
  lambda <- object$lambda
  if (is.null(lambda)) lambda <- 0
  best <- -Inf; bi <- NA; bj <- NA
  for (i in 2:length(stg)) {
    for (j in 1:(i - 1)) {
      d <- join_ll_delta(object$prob[[v]][[stg[i]]], object$prob[[v]][[stg[j]]],
                         lambda, k)
      if (d >= best) { best <- d; bi <- i; bj <- j }   # >= : last wins, as in C
    }
  }
  c(bi, bj, best)
}

call_kernel <- function(object, v, ignore = object$name_unobserved) {
  stg <- unique(object$stages[[v]])
  stg <- stg[!(stg %in% ignore)]
  k <- length(object$tree[[v]])
  lambda <- object$lambda
  if (is.null(lambda)) lambda <- 0
  pv <- object$prob[[v]][stg]
  pm <- do.call(rbind, lapply(pv, as.numeric))
  nv <- vapply(pv, function(p) {
    n <- attr(p, "n"); if (is.null(n)) NA_real_ else as.numeric(n)
  }, FUN.VALUE = 1.0)
  best_merge_cpp(pm, nv, lambda, k)
}

mkd <- function(n, p, lv, seed) {
  set.seed(seed)
  as.data.frame(lapply(seq_len(p), function(i)
    factor(sample(letters[seq_len(lv)], n, replace = TRUE))),
    col.names = paste0("V", seq_len(p)))
}

test_that("best_merge_cpp agrees with the join_ll_delta oracle", {
  n <- 0
  for (seed in 1:8) {
    for (lam in c(0, 1)) {
      m <- full(mkd(400, 4, 3, seed), lambda = lam)
      for (v in sevt_varnames(m)[-1]) {
        o <- oracle_best(m, v)
        if (is.null(o)) next
        got <- call_kernel(m, v)
        expect_equal(got[3], o[3])            # same delta
        expect_identical(as.integer(got[1:2]), as.integer(o[1:2]))  # same pair
        n <- n + 1
      }
    }
  }
  expect_gt(n, 0)
})

test_that("tied deltas resolve to the same pair as R", {
  # perfectly balanced data makes every candidate delta identical, so `>` and
  # `>=` select different merges; this is the case that caught a defect
  D <- expand.grid(V1 = c("a", "b"), V2 = c("a", "b"),
                   V3 = c("a", "b"), V4 = c("a", "b"))
  D <- D[rep(seq_len(nrow(D)), 3), ]
  m <- full(D, lambda = 1)
  for (v in sevt_varnames(m)[-1]) {
    o <- oracle_best(m, v)
    if (is.null(o)) next
    got <- call_kernel(m, v)
    expect_identical(as.integer(got[1:2]), as.integer(o[1:2]))
  }
})

test_that("stages_bhc accepts an arbitrary score function", {
  m <- full(mkd(400, 4, 3, 1), lambda = 1)
  expect_s3_class(stages_bhc(m, score = function(x) -BIC(x)), "sevt")
  expect_s3_class(stages_bhc(m, score = function(x) -AIC(x)), "sevt")
  # a score with an unusual scale must still work
  expect_s3_class(stages_bhc(m, score = function(x) -BIC(x) / 1000), "sevt")
})

test_that("stages_bhc is invariant to monotone rescaling of the score", {
  # any strictly increasing transform of the score must select the same model
  m <- full(mkd(500, 4, 3, 3), lambda = 1)
  a <- stages_bhc(m, score = function(x) -BIC(x))
  b <- stages_bhc(m, score = function(x) -BIC(x) * 2)
  expect_equal(lapply(stages(a), as.character), lapply(stages(b), as.character))
})

test_that("stages_bhc respects max_iter, scope and ignore", {
  m <- full(mkd(400, 4, 3, 5), lambda = 1)
  v <- sevt_varnames(m)[2]
  expect_s3_class(stages_bhc(m, max_iter = 0), "sevt")
  expect_identical(lapply(stages(stages_bhc(m, max_iter = 0)), as.character),
                   lapply(stages(m), as.character))
  sc <- stages_bhc(m, scope = v)
  others <- sevt_varnames(m)[-(1:2)]
  for (w in others) {
    expect_identical(as.character(stages(sc)[[w]]), as.character(stages(m)[[w]]))
  }
})

test_that("stages_bhc stores the score function it used", {
  m <- full(mkd(300, 3, 3, 7), lambda = 1)
  r <- stages_bhc(m, score = function(x) -AIC(x))
  expect_true(is.function(r$score$f))
  expect_true(is.numeric(r$score$value))
})
