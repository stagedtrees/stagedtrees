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

test_that("join_unobserved does not grow name_unobserved on repeated calls (A-B5)", {
  m <- full(generate_random_dataset(3, 30), lambda = 1, join_unobserved = FALSE)
  for (i in 1:20) m <- join_unobserved(m, name = "UNOBSERVED")
  expect_equal(length(m$name_unobserved), 1L)
})
