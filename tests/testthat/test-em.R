test_that("sevt_fit_em: max_iter=0 leaves probabilities unchanged (A-B2)", {
  set.seed(42)
  D <- data.frame(
    A = factor(sample(c("a", "b"), 20, TRUE)),
    B = factor(sample(c("x", "y"), 20, TRUE))
  )
  D$B[1] <- NA
  m <- full(D[!is.na(D$B), ], lambda = 1)
  prob_before <- m$prob
  r <- sevt_fit_em(m, data = D, max_iter = 0)
  expect_equal(r$prob, prob_before)
  expect_null(r$fit_em$iter)  # no iterations ran, iter is never set
})

test_that("stages_em: max_iter_em=0 does not call stages_alg (B-B5)", {
  set.seed(42)
  D <- data.frame(
    A = factor(sample(c("a", "b"), 20, TRUE)),
    B = factor(sample(c("x", "y"), 20, TRUE))
  )
  D$B[1] <- NA
  m <- full(D[!is.na(D$B), ], lambda = 1)
  cnt <- 0L
  alg <- function(o, ...) { cnt <<- cnt + 1L; o }
  stages_em(m, data = D, max_iter_em = 0, stages_alg = alg)
  expect_equal(cnt, 0L)
})
