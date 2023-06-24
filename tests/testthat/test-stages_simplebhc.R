test_that("stages_simplebhc", {
  expect_silent(mod <- stages_simplebhc(full(PhDArticles[, 1:4])))
  mod_c <- ceg(mod)
  expect_identical(stndnaming(mod)$stages, mod_c$positions[-1])
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})