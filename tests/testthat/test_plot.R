context("plot base")

DD <- data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
ev <- sevt(x = DD, order = c("B", "A"))


test_that("plot staged event tree", {
  expect_silent(plot(ev))
})


test_that("plot staged event tree cex_label_nodes = 1", {
  expect_silent(plot(ev, cex_label_nodes = 1, pch =1))
})

test_that("plot should return NULL", {
  expect_null(plot(ev))
})


context("plot staged trees")

DD <- data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
DD <- cbind(DD, generate_random_dataset(6, 4))
mod <- sevt(DD, full = TRUE)

test_that("plot should accept col = 'stages' ", {
  expect_silent(plot(mod, col = "stages"))
})


test_that("plot should accept col = function() ", {
  expect_silent(plot(mod, col = function(s) {
    return(rep(2, length(s)))
  }))
})

test_that("text.sevt works", {
  expect_silent({
    plot(mod)
    text(mod)
    })
})

context("barplot stages")

test_that("barplot.sevt should exit if not fitted", {
  expect_error(barplot(mod, "B"))
})

mod <- sevt_fit(mod, lambda = 1, data = DD)

test_that("barplot.sevt should accept col = 'stages'", {
  expect_silent(barplot(mod, "B", col = "stages"))
})

test_that("barplot.sevt should accept col = function()", {
  expect_silent(barplot(mod, "B", col = function(s) {
    return(grDevices::hcl.colors(length(s)))
  }))
})


test_that("barplot.sevt should accept col = LL", {
  expect_silent(barplot(mod, "B", col = NULL))
})


test_that("barplot.sevt with legend.text=TRUE", {
  expect_silent(barplot(mod, "B", legend.text = TRUE))
})

context("plot ceg")

test_that("plot.ceg", {
  expect_silent(plot(ceg(mod)))
})

test_that("plot.ceg", {
  expect_silent(plot(ceg(mod), col = "stages"))
})

context("make_stages_col")

test_that("make_stages_col with NULL", {
  expect_silent(cols <- make_stages_col(mod))
  expect_is(cols, "list")
  expect_length(cols, 8)
  expect_silent(plot(mod, col = cols))
})


test_that("make_stages_col with list of stages colors", {
  expect_silent(cols <- make_stages_col(mod))
  expect_silent(cols <- make_stages_col(mod, cols))
  expect_is(cols, "list")
  expect_length(cols, 8)
  expect_silent(plot(mod, col = cols))
})

test_that("make_stages_col with function", {
  expect_silent(cols <- make_stages_col(mod, col = function(stages){
    hcl.colors(n = length(stages))
  }))
  expect_is(cols, "list")
  expect_length(cols, 8)
  expect_silent(plot(mod, col = cols))
})



