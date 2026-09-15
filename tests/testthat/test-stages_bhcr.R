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

test_that("bhcr", {
  expect_message(mod <- stages_bhcr(f, max_iter = 10, trace = 2))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("stages_bhcr returns input unchanged on 1-variable model (B-B2)", {
  m <- full(data.frame(X = factor(c("a", "b", "a"))), lambda = 1)
  expect_no_error(stages_bhcr(m, max_iter = 5))
  expect_equal(stages_bhcr(m, max_iter = 5), m)
})
