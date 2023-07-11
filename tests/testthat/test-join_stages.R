## join_stages

test_that("join_stages works as expected", {
  mod <- sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), full = TRUE)
  expect_silent(mod1 <- join_stages(mod, "B", "1", "2"))
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("1", "1"))
})

test_that("join_stages works as expected on tree with probs", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), q = 0)
  expect_silent(mod1 <- join_stages(mod, "B", "1", "2"))
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("1", "1"))
})

test_that("join_stages works as expected on fitted tree", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), q = 0)
  mod <- sevt_fit(mod, sample_from(mod, 100), lambda = 0)
  expect_silent(mod1 <- join_stages(mod, "B", "1", "2"))
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("1", "1"))
})
