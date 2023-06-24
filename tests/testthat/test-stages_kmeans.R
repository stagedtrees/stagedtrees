test_that("stages_kmeans", {
  fl <- full(PhDArticles[, 1:4], lambda = 0.5)
  expect_silent(mod <- stages_kmeans(join_unobserved(fl, name = "NA"),
    k = c(2, 3), ignore = "NA"
  ))
  expect_silent(mod <- stages_kmeans(join_unobserved(fl, name = "NA"),
    k = c(2, 3),
    scope = "Kids"
  ))
  expect_silent(mod <- stages_kmeans(fl, transform = function(x) x^2))
  expect_silent(mod <- stages_kmeans(fl, transform = NULL))
  expect_silent(mod <- stages_kmeans(fl))
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})
