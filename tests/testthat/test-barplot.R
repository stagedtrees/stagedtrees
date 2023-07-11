mod <- sevt(list(
  "A" = c("aa", "aaa", "a"),
  "B" = c("b", "bbb")
))

test_that("barplot.sevt should exit if not fitted", {
  expect_error(barplot(mod, "B"))
})

mod <- random_sevt(list(
  "A" = c("aa", "aaa", "a"),
  "B" = c("b", "bbb")
))

test_that("barplot.sevt should accept col as a list", {
  col <- make_stages_col(mod)
  expect_silent(barplot(mod, "B", col = col))
})

test_that("barplot.sevt fails if col has wrong names", {
  col <- make_stages_col(mod)
  names(col$"B") <- paste0(seq(unique(mod$stages$B)), "X")
  expect_error(barplot(mod, "B", col = col))
})

test_that("barplot.sevt should accept col = 'stages'", {
  expect_silent(barplot(mod, "B", col = "stages"))
})

test_that("barplot.sevt should accept col = function()", {
  expect_silent(barplot(mod, "B", col = function(s) {
    return(grDevices::hcl.colors(length(s)))
  }))
})


test_that("barplot.sevt should accept col = NULL", {
  expect_silent(barplot(mod, "B", col = NULL))
})


test_that("barplot.sevt with legend.text = TRUE", {
  expect_silent(barplot(mod, "B", legend.text = TRUE))
})
