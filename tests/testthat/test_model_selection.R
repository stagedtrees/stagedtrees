context("model selection")

data("PhDArticles")
f <- full(PhDArticles[, 1:4])
ind <- indep(PhDArticles[, 1:4])
fl <- full(PhDArticles[, 1:4], lambda = 1)

test_that("hc_sevt from full", {
  expect_message(mod <- hc_sevt(f, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("hc_sevt from indep", {
  expect_message(mod <- hc_sevt(ind, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bhc_sevt", {
  expect_message(mod <- bhc_sevt(f, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("fbhc_sevt", {
  expect_message(mod <- fbhc_sevt(f, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bhcr_sevt", {
  expect_message(mod <- bhcr_sevt(f, max_iter = 10, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bj_sevt", {
  expect_message(mod <- bj_sevt(f, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("join_zero", {
  expect_message(mod <- join_zero(f, trace = 2, name = "zeros"))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("naive_sevt", {
  expect_silent(mod <- naive_sevt(fl))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("hclust_sevt", {
  expect_silent(mod <- hclust_sevt(join_zero(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- hclust_sevt(join_zero(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- hclust_sevt(fl))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("kmeans_sevt", {
  expect_silent(mod <- kmeans_sevt(join_zero(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- kmeans_sevt(join_zero(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- kmeans_sevt(fl, transform = function(x) x^2))
  expect_silent(mod <- kmeans_sevt(fl, transform = NULL))
  expect_silent(mod <- kmeans_sevt(fl)) 
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})