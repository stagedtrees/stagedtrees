test_that("stages_simplebhc", {
  expect_silent(mod <- stages_simplebhc(full(PhDArticles[, 1:4])))
  mod_c <- ceg(mod)
  vars <- names(mod$tree)
  expect_identical(stndnaming(mod)$stages[vars[-1]],
                   mod_c$positions[vars[-1]])
  ll1 <- logLik(mod)
  mod$ll <- NULL
  ll2 <- logLik(mod)
  expect_equal(ll1, ll2)
})
