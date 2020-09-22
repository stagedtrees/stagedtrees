context("model selection")

data("PhDArticles")
f <- full(PhDArticles[, 1:4])
ind <- indep(PhDArticles[, 1:4])
fl <- full(PhDArticles[, 1:4], lambda = 1)

test_that("hc from full", {
  expect_message(mod <- hc(f, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("hc from indep", {
  expect_message(mod <- hc(ind, max_iter = 3, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bhc", {
  expect_message(mod <- bhc(f, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("fbhc", {
  expect_message(mod <- fbhc(f, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bhcr", {
  expect_message(mod <- bhcr(f, max_iter = 10, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bj", {
  expect_message(mod <- bj(f, trace = 2))
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





test_that("stages_hclust", {
  expect_silent(mod <- stages_hclust(join_zero(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- stages_hclust(join_zero(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- stages_hclust(fl))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("stages_kmeans", {
  expect_silent(mod <- stages_kmeans(join_zero(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- stages_kmeans(join_zero(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- stages_kmeans(fl, transform = function(x) x^2))
  expect_silent(mod <- stages_kmeans(fl, transform = NULL))
  expect_silent(mod <- stages_kmeans(fl)) 
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})