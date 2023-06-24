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

test_that("logLik after joining (NA)", {
  DD <- generate_linear_dataset(5, 10)

  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  DD[sample(10, size = 2), sample(5, 1)] <- NA
  DD[sample(10, size = 2), sample(5, 1)] <- NA
  DD[sample(10, size = 2), sample(5, 1)] <- NA

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
