mod <- random_sevt(list(c("1", "0"),
                        c("a", "aa", "aaa"),
                        c('bb', "1"),
                        c('-1', "1"),
                        c('la', "lala", "lalala")))

test_that("sample size is correct", {
  data <- sample_from(mod, 27)
  expect_equal(nrow(data), 27)
})

test_that("sample_from should return a data.frame with factors", {
  data <- sample_from(mod, 27)
  expect_s3_class(data, "data.frame")
  expect_s3_class(data$V1, "factor")
  expect_s3_class(data$V2, "factor")
  expect_s3_class(data$V3, "factor")
})


test_that("variables number and names are correct", {
  data <- sample_from(mod, 35)
  expect_equal(colnames(data), sevt_varnames(mod))
  expect_equal(colnames(data), names(mod$tree))
  
})

test_that("sampling with seed", {
  data <- sample_from(mod, 35, seed = 23)
  expect_equal(colnames(data), names(mod$tree))
  expect_equal(nrow(data), 35)
})



test_that("sampling of 1 observation works", {
  data <- sample_from(mod, 1)
  expect_equal(colnames(data), names(mod$tree))
  expect_equal(nrow(data), 1)
})


test_that("sampling of 0 or <0 should throw error", {
  expect_error(sample_from(mod, 0))
})

prob <- mod$prob
mod$prob <- NULL

test_that("sampling from a non fitted model shoudl throw error", {
  expect_error(sample_from(mod, 10))
})

# restore probabilities
mod$prob <- prob


test_that("sampling from a non sevt object shoudl throw error", {
  class(mod) <- "ajsjhhsajh"
  expect_error(sample_from(mod, 10))
})
