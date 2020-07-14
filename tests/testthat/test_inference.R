context("predict.sevt")


DD <- generate_linear_dataset(5, 100)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 1)

test_that("predict class values", {
  expect_silent(pr <- predict(sev, DD))
  expect_true(all(levels(pr) == levels(DD$C)))
  expect_silent(pr <- predict(sev, class = "X3", DD))
  expect_true(all(levels(pr) == levels(DD$X3)))
})


test_that("predict probabilities", {
  pr <- predict(sev, DD, prob = TRUE, log = FALSE)
  expect_true(all(pr >= 0))
  pr <- predict(sev, DD, class = "X4", prob = TRUE, log = FALSE)
  expect_true(all(pr >= 0))
})


test_that("predict log-probabilities", {
  expect_silent(pr <- predict(sev, DD, prob = TRUE, log = TRUE))
  expect_silent(pr <- predict(sev, DD, class = "X4", prob = TRUE, log = TRUE))
})


test_that("predict with no data", {
  expect_error(pr <- predict(sev))
  sev$data <- DD
  expect_silent(pr <- predict(sev, class = "X4"))
})

context("prob.sevt")

test_that("probabilities are ok", {
  DD <- generate_linear_dataset(5, 100)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  sev <- full(DD, lambda = 1)
  pr <- prob.sevt(sev, c(X1 = "-1", X3 = "pppp"))
  expect_true(all(pr >= 0))
  pr <- prob.sevt(sev, c(C = "a"))
  expect_true(all(pr >= 0))
  pr <- prob.sevt(sev, c(X1 = "1"))
  expect_true(all(pr >= 0))
  pr <- prob.sevt(sev, c(C = "a"))
  expect_true(all(abs(pr - sev$prob$C$`1`["a"]) < 1e-10))
})

test_that("probabilities are positive", {
  DD <- generate_linear_dataset(5, 100)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  sev <- full(DD, lambda = 1)
  pr <- prob.sevt(sev, DD)
  expect_true(all(pr >= 0))
})


test_that("probabilities sum to 1", {
  DD <- generate_linear_dataset(5, 100)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  sev <- full(DD, lambda = 1)
  pr <- prob.sevt(sev, expand.grid(sev$tree))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
  pr <- prob.sevt(sev, expand.grid(sev$tree[1:3]))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
})


context("logLik.sevt")

test_that("logLik after joining", {
  DD <- generate_linear_dataset(5, 10)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")

  invisible(replicate(20, {
    v <- sample(colnames(DD)[-1], size = 1)
    lambda <- sample(c(0, 1, 2), size = 1)
    sev <- full(DD, lambda = lambda)
    stages <- sample(sev$stages[[v]], size = 2, replace = FALSE)
    sev <- join_stages(sev, v, stages[1], stages[2])
    ll1 <- logLik(sev)
    sev$ll <- NULL
    ll2 <- logLik(sev)
    expect_equal(ll1, ll2)
  }))
})
