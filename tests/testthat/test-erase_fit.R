test_that("erase_fit erase the fit", {
  mod <- full(Titanic)
  mod0 <- erase_fit(mod)
  expect_null(mod0$ll)
  expect_null(mod0$prob)
})
