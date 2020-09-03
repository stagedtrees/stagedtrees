context("model selection")

data("PhDArticles")
f <- full(PhDArticles[, 1:4])
ind <- indep(PhDArticles[, 1:4])
fl <- full(PhDArticles[, 1:4], lambda = 1)

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


test_that("bhcr.sevt", {
  expect_message(mod <- bhcr.sevt(f, max_iter = 10, trace = 2))
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


test_that("join_zero_counts", {
  expect_message(mod <- join_zero_counts(f, trace = 2, name = "zeros"))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("naive.sevt", {
  expect_silent(mod <- naive.sevt(fl))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("hclust.sevt", {
  expect_silent(mod <- hclust.sevt(join_zero(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- hclust.sevt(join_zero(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- hclust.sevt(fl))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("kmeans.sevt", {
  expect_silent(mod <- kmeans.sevt(join_zero(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- kmeans.sevt(join_zero(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- kmeans.sevt(fl, transform = function(x) x^2))
  expect_silent(mod <- kmeans.sevt(fl, transform = NULL))
  expect_silent(mod <- kmeans.sevt(fl)) 
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})