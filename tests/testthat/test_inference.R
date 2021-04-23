context("prob")

test_that("probabilities are ok", {
  DD <- generate_linear_dataset(5, 100)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  sev <- full(DD, lambda = 1)
  pr <- prob(sev, c(X1 = "-1", X3 = "pppp"))
  expect_true(all(pr >= 0))
  pr <- prob(sev, c(C = "a"))
  expect_true(all(pr >= 0))
  pr <- prob(sev, c(X1 = "1"))
  expect_true(all(pr >= 0))
  pr <- prob(sev, c(C = "a"))
  expect_true(all(abs(pr - sev$prob$C$`1`["a"]) < 1e-10))
})

test_that("probabilities are positive", {
  DD <- generate_linear_dataset(5, 100)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  sev <- full(DD, lambda = 1)
  pr <- prob(sev, DD)
  expect_true(all(pr >= 0))
})


test_that("probabilities sum to 1", {
  DD <- generate_linear_dataset(5, 100)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  sev <- full(DD, lambda = 1)
  pr <- prob(sev, expand.grid(sev$tree))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
  pr <- prob(sev, expand.grid(sev$tree[1:3]))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
})

test_that("probability for object with wrong order in $prob", {
  DD <- generate_linear_dataset(5, 100)
  sev <- full(DD, lambda = 1) 
  pr <- prob(sev, c(X4 = "-1"))
  sev$prob <- lapply(sev$prob, function(pp) pp[sample(seq_along(pp))])
  sev$prob <- sev$prob[sample(seq_along(sev$prob))]
  expect_equal(pr, prob(sev, c(X4 = "-1")))
})

test_that("probability for object with wrong order in $stages", {
  DD <- generate_linear_dataset(5, 100)
  sev <- full(DD, lambda = 1) 
  pr <- prob(sev, c(X4 = "-1"))
  sev$stages <- sev$stages[sample(seq_along(sev$stages))]
  expect_equal(pr, prob(sev, c(X4 = "-1")))
})


context("logLik.sevt")

test_that("logLik for 1 var model", {
  sev <- full(data.frame(x = c("A", "B", "A", "A")))
  ll <- sev$ll
  sev$ll <- NULL
  expect_equal(logLik(sev), ll)
})

test_that("logLik after joining", {
  DD <- generate_linear_dataset(5, 10)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")

  invisible(replicate(20, {
    v <- sample(colnames(DD)[-1], size = 1)
    lambda <- sample(c(0, 1, 2), size = 1)
    sev <- full(DD, lambda = lambda)
    stages <- sample(unique(sev$stages[[v]]), size = 2, replace = FALSE)
    sev <- join_stages(sev, v, stages[1], stages[2])
    ll1 <- logLik(sev)
    sev$ll <- NULL
    ll2 <- logLik(sev)
    expect_equal(ll1, ll2)
  }))
})
