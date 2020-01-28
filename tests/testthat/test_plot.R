context("plot base")

DD <- data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
ev <- staged_ev_tree(x = DD, order = c("B", "A"))


test_that("plot staged event tree", {
  expect_silent(plot(ev))
})

test_that("plot should return NULL", {
  expect_null(plot(ev))
})


context("plot staged trees")

DD <- data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
DD <- cbind(DD, generate_random_dataset(6, 4))
mod <- full(DD, fit = FALSE)

test_that("plot should accept col = 'stages' ", {
  expect_silent(plot(mod, col = "stages"))
})


test_that("plot should accept col = function() ", {
  expect_silent(plot(mod, col = function(s) {
    return(rep(2, length(s)))
  }))
})

context("barplot stages")

test_that("barplot_stages should exit if not fitted", {
  expect_error(barplot_stages(mod, "B"))
})

mod <- sevt.fit(mod, lambda = 1, data = DD)

test_that("barplot_stages should accept col = 'stages'", {
  expect_silent(barplot_stages(mod, "B", col = "stages"))
})


test_that("barplot_stages should accept col = function()", {
  expect_silent(barplot_stages(mod, "B", col = function(s) {
    return(grDevices::hcl.colors(length(s)))
  }))
})
