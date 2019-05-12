context("sampling")

DD <- generate_linear_dataset(5,100)
mod <- indep(DD)
prob <- mod$prob

test_that("sample size is correct", {
  D <- sample.sevt(mod, 27)
  expect_equal(nrow(D), 27)
})


test_that("variables number and names are correct", {
  D <- sample.sevt(mod, 35)
  expect_equal(colnames(D),  colnames(DD))
})



test_that("sampling of 1 observation works", {
  D <- sample.sevt(mod, 1)
  expect_equal(colnames(D),  colnames(DD))
  expect_equal(nrow(D),  1)
})


test_that("sampling of 0 or <0 should throw error", {
  expect_error(D <- sample.sevt(mod, 0))
})

mod$prob <- NULL

test_that("sampling from a non fitted model shoudl throw error", {
  expect_error(D <- sample.sevt(mod, 10))
})

mod$prob <- prob



test_that("sampling from a non sevt object shoudl throw error", {
  class(mod) <- "ajsjhhsajh"
  expect_error(D <- sample.sevt(mod, 10))
})


