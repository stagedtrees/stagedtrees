modf <- full(Titanic)
mod1 <- indep(Titanic)
mod2 <- stages_bj(modf, thr = 0.1)
mod3 <- full(generate_linear_dataset(4, 10))
mod4 <- full(generate_linear_dataset(2, 10))

test_that("lr_test throws error with not nested models I", {
  expect_s3_class(lr_test(mod1), "anova")
  expect_s3_class(lr_test(mod2), "anova")
  expect_error(lr_test(mod2, mod1))
})

test_that("lr_test throws error with not nested models II", {
  expect_s3_class(lr_test(mod1), "anova")
  expect_s3_class(lr_test(modf), "anova")
  expect_error(lr_test(modf, mod1))
})

test_that("lr_test throws error with model on different variables", {
  expect_s3_class(lr_test(mod1), "anova")
  expect_s3_class(lr_test(mod3), "anova")
  expect_s3_class(lr_test(mod4), "anova")
  expect_error(lr_test(mod1, mod3))
  expect_error(lr_test(mod1, mod4))
})

test_that("lr_test works with one", {
  expect_s3_class(lr_test(mod2), class = "anova")
})

test_that("lr_test works with two nested models", {
  expect_s3_class(lr_test(mod1, mod2), class = "anova")
})


test_that("lr_test works with three models ", {
  expect_s3_class(lr_test(mod1, mod2, modf), class = "anova")
})
