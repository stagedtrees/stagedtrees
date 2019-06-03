context("model selection")

data("PhDArticles")
f <- full(PhDArticles[,1:4])
ind <- indep(PhDArticles[,1:4])


test_that("hc.sevt from full", {
   expect_message(mod <- hc.sevt(f, max_iter = 3, trace = 2))  
   ll1 <- logLik(mod)
   mod$ll <- NULL
   ll2 <- logLik(mod)
   expect_equal(ll1, ll2)
})

test_that("hc.sevt from indep", {
  expect_message(mod <- hc.sevt(ind, max_iter = 3, trace = 2))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bhc.sevt", {
  expect_message(mod <- bhc.sevt(f, max_iter = 5, trace = 2))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("fbhc.sevt", {
  expect_message(mod <- fbhc.sevt(f, max_iter = 5, trace = 2))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bj.sevt", {
  expect_message(mod <- bj.sevt(f, max_iter = 5, trace = 2))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})
