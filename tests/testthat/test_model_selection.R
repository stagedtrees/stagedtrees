context("model selection")

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


test_that("bhc", {
  expect_silent(mod <- stages_bhc(f, max_iter = 5, scope = "Kids"))
  expect_message(mod <- stages_bhc(f, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("bhc (na)", {
  expect_silent(mod <- stages_bhc(fna, max_iter = 5, scope = "Kids"))
  expect_message(mod <- stages_bhc(fna, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("fbhc", {
  expect_silent(mod <- stages_fbhc(f, max_iter = 5, scope = "Kids"))
  expect_message(mod <- stages_fbhc(f, max_iter = 5, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bhcr", {
  expect_message(mod <- stages_bhcr(f, max_iter = 10, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("bj", {
  expect_silent(mod <- stages_bj(f, scope = "Kids"))
  expect_message(mod <- stages_bj(f, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("bj (na)", {
  expect_silent(mod <- stages_bj(fna, scope = "Kids"))
  expect_message(mod <- stages_bj(fna, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})


test_that("join.unobserved", {
  expect_message(mod <- join_unobserved(f, trace = 2, name = "zeros"))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("join.unobserved (NAs)", {
  expect_message(mod <- join_unobserved(fna, trace = 2, name = "zeros"))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})





test_that("stages_hclust", {
  expect_silent(mod <- stages_hclust(join_unobserved(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- stages_hclust(join_unobserved(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- stages_hclust(fl))  
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("stages_kmeans", {
  expect_silent(mod <- stages_kmeans(join_unobserved(fl, name = "NA"), 
                                   k = c(2,3), ignore = "NA"))
  expect_silent(mod <- stages_kmeans(join_unobserved(fl, name = "NA"), k = c(2,3), 
                                   scope = "Kids"))
  expect_silent(mod <- stages_kmeans(fl, transform = function(x) x^2))
  expect_silent(mod <- stages_kmeans(fl, transform = NULL))
  expect_silent(mod <- stages_kmeans(fl)) 
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})