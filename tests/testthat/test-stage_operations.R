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

test_that("rename_stage aborts when new already exists (C3)", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), q = 0)
  mod <- stndnaming(mod)
  stages_B <- unique(mod$stages$B)
  # rename onto an existing stage must error, not silently merge/overwrite
  expect_error(
    rename_stage(mod, "B", stages_B[1], stages_B[2]),
    regexp = "already exists"
  )
})

test_that("rename_stage noop when new == stage (C3)", {
  mod <- random_sevt(list(A = c("a","aa"), B = c("b","bb")), q = 0)
  mod <- stndnaming(mod)
  mod2 <- rename_stage(mod, "B", "1", "1")
  expect_identical(mod2$stages, mod$stages)
})

test_that("rename_stage clears cached ll (C3)", {
  data(Titanic)
  m <- full(Titanic, lambda = 1)
  expect_false(is.null(m$ll))
  m2 <- rename_stage(m, "Age", unique(m$stages$Age)[1], "NEWNAME")
  expect_null(m2$ll)
})
