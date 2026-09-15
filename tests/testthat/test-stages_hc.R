data("PhDArticles")
### introduce some NAs
DDna <- PhDArticles
DDna[sample(nrow(DDna), 10), 1] <- NA
DDna[sample(nrow(DDna), 10), 2] <- NA
DDna[sample(nrow(DDna), 10), 3] <- NA

f <- full(PhDArticles[, 1:4])
fna <- full(DDna[, 1:4])
ind <- indep(PhDArticles[, 1:4])
indna <- indep(DDna[, 1:4])
fl <- full(PhDArticles[, 1:4], lambda = 1)

test_that("hc from full", {
  expect_silent(mod <- stages_hc(f, max_iter = 3, scope = "Kids"))
  expect_message(mod <- stages_hc(f, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("hc from full (NAs)", {
  expect_silent(mod <- stages_hc(fna, max_iter = 3, scope = "Kids"))
  expect_message(mod <- stages_hc(fna, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("hc from indep", {
  expect_silent(mod <- stages_hc(ind, max_iter = 3, scope = "Kids"))
  expect_message(mod <- stages_hc(ind, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("hc from indep (NAs)", {
  expect_silent(mod <- stages_hc(indna, max_iter = 3, scope = "Kids"))
  expect_message(mod <- stages_hc(indna, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})
## best_move_cpp computes, in closed form, the change a single-situation move
## would cause. The oracle is an actual refit: perform the move, refit the
## variable, and read off the true change in log-likelihood and df.

refit_truth <- function(object, v, i, target) {
  ll0 <- as.numeric(logLik(object))
  df0 <- attr(logLik(object), "df")
  o <- object
  o$stages[[v]][i] <- target
  o$ll <- NULL
  o <- sevt_fit(o, scope = v)
  o$ll <- NULL
  c(as.numeric(logLik(o)) - ll0, attr(logLik(o), "df") - df0)
}

call_move <- function(object, v, lambda) {
  stages <- object$stages[[v]]
  us <- unique(stages)
  ct <- as.matrix(object$ctables[[v]])
  storage.mode(ct) <- "double"
  asg <- match(stages, us) - 1L
  list(cand = best_move_cpp(ct, as.integer(asg), length(us), lambda), us = us)
}

mkd <- function(n, p, lv, seed) {
  set.seed(seed)
  as.data.frame(lapply(seq_len(p), function(i)
    factor(sample(letters[seq_len(lv)], n, replace = TRUE))),
    col.names = paste0("V", seq_len(p)))
}

test_that("best_move_cpp deltas match an actual refit", {
  n <- 0
  for (seed in 1:5) {
    for (lam in c(0, 1)) {
      m <- full(mkd(400, 4, 3, seed), lambda = lam)
      for (v in sevt_varnames(m)[-1]) {
        if (length(unique(m$stages[[v]])) < 2) next
        r <- call_move(m, v, lam)
        for (row in seq_len(nrow(r$cand))) {
          i <- r$cand[row, 1]
          target <- if (r$cand[row, 2] == 0) new_label(r$us) else r$us[r$cand[row, 2]]
          tr <- refit_truth(m, v, i, target)
          expect_equal(tr[1], r$cand[row, 3])   # log-likelihood change
          expect_equal(tr[2], r$cand[row, 4])   # degrees of freedom change
          n <- n + 1
        }
      }
    }
  }
  expect_gt(n, 0)
})

test_that("best_move_cpp returns one representative per df group", {
  m <- full(mkd(400, 4, 3, 2), lambda = 1)
  k <- length(m$tree[[2]])
  for (v in sevt_varnames(m)[-1]) {
    if (length(unique(m$stages[[v]])) < 2) next
    cand <- call_move(m, v, 1)$cand
    expect_lte(nrow(cand), 3L)
    expect_equal(length(unique(cand[, 4])), nrow(cand))   # groups are distinct
    expect_true(all(cand[, 4] %in% c(-(k - 1), 0, k - 1)))
  }
})

test_that("stages_hc never decreases the score", {
  for (seed in 1:4) {
    m <- full(mkd(400, 4, 3, seed), lambda = 1)
    before <- -BIC(m)
    after <- -BIC(stages_hc(m))
    expect_gte(after, before)
  }
})

test_that("stages_hc works with an arbitrary score and respects arguments", {
  m <- full(mkd(400, 4, 3, 1), lambda = 1)
  expect_s3_class(stages_hc(m, score = function(x) -AIC(x)), "sevt")
  # max_iter = 0 must leave the model untouched
  expect_identical(lapply(stages(stages_hc(m, max_iter = 0)), as.character),
                   lapply(stages(m), as.character))
  # scope must leave other variables untouched
  v <- sevt_varnames(m)[2]
  sc <- stages_hc(m, scope = v)
  for (w in sevt_varnames(m)[-(1:2)]) {
    expect_identical(as.character(stages(sc)[[w]]), as.character(stages(m)[[w]]))
  }
})

test_that("stages_hc leaves ignored stages untouched", {
  # sparse data so that unobserved situations genuinely exist
  m <- full(mkd(60, 4, 3, 3), lambda = 1, join_unobserved = TRUE)
  checked <- 0
  r <- stages_hc(m)
  for (v in sevt_varnames(m)[-1]) {
    keep <- m$stages[[v]] %in% m$name_unobserved
    if (!any(keep)) next
    expect_identical(r$stages[[v]][keep], m$stages[[v]][keep])
    checked <- checked + 1
  }
  expect_gt(checked, 0)   # the test must actually exercise something
})
