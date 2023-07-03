test_that("stages_hclust", {
  fl <- full(PhDArticles[, 1:4], lambda = 1)
  expect_silent(mod <- stages_hclust(join_unobserved(fl, name = "NA"),
    k = c(2, 3), ignore = "NA"
  ))
  expect_silent(mod <- stages_hclust(join_unobserved(fl, name = "NA"),
    k = c(2, 3),
    scope = "Kids"
  ))
  expect_silent(mod <- stages_hclust(fl))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("stages_hclust with unobserved paths", {
  fl <- full(PhDArticles[, 1:5], lambda = 0)
  expect_silent(mod <- stages_hclust(join_unobserved(fl, name = "NA"),
    k = c(2, 3), ignore = "NA"
  ))
  expect_silent(mod <- stages_hclust(join_unobserved(fl, name = "NA"),
    k = c(2, 3),
    scope = "Kids"
  ))
  expect_silent(mod <- stages_hclust(fl))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})

test_that("stages_hclust fails if not numeric k", {
  mod <- full(Titanic, lambda = 1)
  expect_error(stages_hclust(mod, k = "a"))
})

test_that("stages_hclust fails with a non-string distance", {
  mod <- full(Titanic, lambda = 1)
  expect_error(stages_hclust(mod, distance = "ffff"))
})
