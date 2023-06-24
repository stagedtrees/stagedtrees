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
