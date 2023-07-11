## set_stage

test_that("set_stage is not implemented", {
  mod <- sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ))
  expect_warning(set_stage(mod, c(A = "a", B = "b"), "new"))
})

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

## split_stage_random

test_that("split_stage_random works", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), q = 2)
  mod <- stndnaming(mod)
  mod <- sevt_fit(mod, sample_from(mod, 100), lambda = 0)
  expect_silent(mod1 <- split_stage_random(mod, "B", "1"))
  expect_s3_class(mod1, "sevt")
})

## rename_stage

test_that("rename_stage works as expected", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_silent(mod1 <- rename_stage(mod,
    var = "B",
    stage = "2", new = "NEW"
  ))
  expect_s3_class(mod1, "sevt")
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("1", "NEW"))
})

test_that("rename_stage works as expected (with probs)", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), q = 0)
  mod <- stndnaming(mod)
  mod <- sevt_fit(mod, sample_from(mod, 100), lambda = 0)
  expect_silent(mod1 <- rename_stage(mod,
                                     var = "B",
                                     stage = "1", new = "NEW"
  ))
  expect_s3_class(mod1, "sevt")
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("NEW", "2"))
})

test_that("rename_stage fails if stage is not a stage of var", {
  mod <- sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ))
  expect_error(rename_stage(mod, "B", "asfnafj", "new"))
})

## stages

test_that("stages function", {
  mod <- random_sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ))
  expect_equal(stages(mod, "B"), mod$stages$B)
  expect_equal(stages(mod, "C"), mod$stages$C)
})

test_that("stages function get all the stages", {
  mod <- random_sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ))
  expect_identical(stages(mod), mod$stages)
})
