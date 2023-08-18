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


test_that("join_all joins all stages", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb"),
    C = c("c", "cc")
  ), q = 0.5)
  mod1 <- join_all(mod, "C", unique(mod$stages$C))
  expect_length(unique(mod1$stages$C), 1)
})


test_that("join_all works for 2 stages", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb"),
    C = c("c", "cc")
  ), q = 0)
  mod1 <- join_all(mod, "C", c("1", "3"))
  mod2 <- join_stages(mod, "C", "1", "3")
  expect_identical(mod1, mod2)
})

test_that("join_all ignores stages in ignore argument", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb"),
    C = c("c", "cc")
  ), q = 0)
  mod1 <- join_all(mod, "C", c("1", "3", "4"), ignore = "4")
  mod2 <- join_stages(mod, "C", "1", "3")
  expect_identical(mod1, mod2)
})
