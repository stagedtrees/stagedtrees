context("sampling")

DD <- generate_linear_dataset(5, 100)
mod <- indep(DD, lambda = 0)
prob <- mod$prob

test_that("sample size is correct", {
  D <- sample_from(mod, 27)
  expect_equal(nrow(D), 27)
})

test_that("sample_from should return a data.frame with factors", {
  D <- sample_from(mod, 27)
  expect_is(D, "data.frame")
  expect_is(D$C, "factor")
  expect_is(D$X3, "factor")
  expect_is(D$X5, "factor")
})


test_that("variables number and names are correct", {
  D <- sample_from(mod, 35)
  expect_equal(colnames(D), colnames(DD))
})

test_that("sampling with seed", {
  D <- sample_from(mod, 35, seed = 23)
  expect_equal(colnames(D), colnames(DD))
  expect_equal(nrow(D), 35)
})



test_that("sampling of 1 observation works", {
  D <- sample_from(mod, 1)
  expect_equal(colnames(D), colnames(DD))
  expect_equal(nrow(D), 1)
})


test_that("sampling of 0 or <0 should throw error", {
  expect_error(D <- sample_from(mod, 0))
})

mod$prob <- NULL

test_that("sampling from a non fitted model shoudl throw error", {
  expect_error(D <- sample_from(mod, 10))
})

mod$prob <- prob



test_that("sampling from a non sevt object shoudl throw error", {
  class(mod) <- "ajsjhhsajh"
  expect_error(D <- sample_from(mod, 10))
})
