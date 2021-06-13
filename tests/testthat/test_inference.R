context("prob")

DD <- generate_linear_dataset(5, 100)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 1)

test_that("probabilities are ok", {
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
  pr <- prob(sev, DD)
  expect_true(all(pr >= 0))
})

test_that("conditional probabilities are positive", {
  pr <- prob(sev, DD[, c(1,3,4)], conditional_on = DD[, c(2,5)])
  expect_true(all(pr >= 0))
})

test_that("conditional probabilities are positive", {
  pr <- prob(sev, DD[, c(1,3,4)], conditional_on = c(X1 = "1"))
  expect_true(all(pr >= 0))
})


test_that("probabilities sum to 1", {
  pr <- prob(sev, expand.grid(sev$tree))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
  pr <- prob(sev, expand.grid(sev$tree[1:3]))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
})

test_that("conditional probabilities sum to 1", {
  pr <- prob(sev, expand.grid(sev$tree))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
  pr <- prob(sev, expand.grid(sev$tree[1:3]), 
             conditional_on = c(X3 = 'pppp', X5 = '-1') )
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
})

test_that("probability for object with wrong order in $prob", {
  pr <- prob(sev, c(X4 = "-1"))
  sev$prob <- lapply(sev$prob, function(pp) pp[sample(seq_along(pp))])
  sev$prob <- sev$prob[sample(seq_along(sev$prob))]
  expect_equal(pr, prob(sev, c(X4 = "-1")))
})

test_that("probability for object with wrong order in $stages", {
  pr <- prob(sev, c(X4 = "-1"))
  sev$stages <- sev$stages[sample(seq_along(sev$stages))]
  expect_equal(pr, prob(sev, c(X4 = "-1")))
})

test_that("prob should raise error if x and conditional_on has same names", {
  ### this first check is just to check that an error is not from something else
  expect_length(prob(sev, c(X4 = "-1"), conditional_on = c(X1 = "1")), 1)
  expect_length(prob(sev, c(X4 = "-1")), 1)
  ## now we test what we want
  expect_error(prob(sev, c(X4 = "-1"), conditional_on = c(X1 = "1", X4 = "1")))
  expect_error(prob(sev, data.frame(X4 = c("-1", "1")), conditional_on = c(X1 = "1", X4 = "1")))
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


context("lr_test")

modf <- full(Titanic)
mod1 <- indep(Titanic)
mod2 <- stages_bj(modf, thr = 0.1)
mod3 <- full(generate_linear_dataset(4, 10))
mod4 <- full(generate_linear_dataset(2, 10))

test_that("lr_test throws error with not nested models I", {
  expect_is(lr_test(mod1), "anova")
  expect_is(lr_test(mod2), "anova")
  expect_error(lr_test(mod2, mod1))
})

test_that("lr_test throws error with not nested models II", {
  expect_is(lr_test(mod1), "anova")
  expect_is(lr_test(modf), "anova")
  expect_error(lr_test(modf, mod1))
})

test_that("lr_test throws error with model on different variables", {
  expect_is(lr_test(mod1), "anova")
  expect_is(lr_test(mod3), "anova")
  expect_is(lr_test(mod4), "anova")
  expect_error(lr_test(mod1, mod3))
  expect_error(lr_test(mod1, mod4))
})

test_that("lr_test works with one", {
  expect_is(lr_test(mod2), class = "anova")
})

test_that("lr_test works with two nested models", {
  expect_is(lr_test(mod1, mod2), class = "anova")
})


test_that("lr_test works with three models ", {
  expect_is(lr_test(mod1, mod2, modf), class = "anova")
})


test_that("lr_test throws warning if lambda > 0", {
  mod1a <- sevt_fit(mod1, lambda = 1)
  mod2a <- sevt_fit(mod2, lambda = 2)
  expect_warning(lr_test(mod1a, mod2a))
  expect_warning(lr_test(mod2a))
})

