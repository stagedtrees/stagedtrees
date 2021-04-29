context("confidence intervals function")

test_that("confint1 works properly with indep staged tree", {
  m1 <- indep(PhDArticles)
  ci <- confint1(m1, digits = 6)
  expect_equal(length(ci), expected = length(m1$tree))
  expect_equal(length(ci), expected = length(m1$prob))
  expect_equal(as.numeric(round(ci[[1]][, 1:length(m1$tree[[1]])], 4)), 
               expected = as.numeric(round(m1$prob[[1]][[1]], 4)))
})

test_that("confint1 works properly with full staged tree", {
  m1 <- full(PhDArticles)
  ci <- confint1(m1, digits = 6)
  expect_equal(length(ci), expected = length(m1$tree))
  expect_equal(length(ci), expected = length(m1$prob))
  expect_equal(as.numeric(round(ci[[1]][, 1:length(m1$tree[[1]])], 4)), 
               expected = as.numeric(round(m1$prob[[1]][[1]], 4)))
  a <- b <- NULL
  for(i in 1:length(m1$tree)) {
      a <- c(a, NROW(ci[[i]]))
      b <- c(b, length(m1$prob[[i]]))
  }
  expect_equal(a, expected = b)
})

test_that("confint1 works properly with errors and warnings", {
  m1 <- stages_fbhc(full(PhDArticles))
  ci <- confint1(m1, digits = 6)
  expect_equal(length(ci), expected = length(m1$tree))
  expect_equal(length(ci), expected = length(m1$prob))
  expect_warning(confint1(m1, digits = 6, stage = 1))
  expect_warning(confint1(m1, digits = 6, stage = c(1:10)))
  expect_warning(confint1(m1, digits = 4, var = c("Kids", "Married"), stage = c(1:10)))
  expect_error(confint1(m1, digits = 4, var = c("Kids"), stage = c(1:100)))
})

test_that("confint1 works properly with different parameters", {
  m1 <- stages_fbhc(full(PhDArticles))
  expect_silent(confint1(m1, digits = 6))
  expect_silent(confint1(m1, level = 0.5))
  expect_silent(confint1(m1, sides = "left", var = names(m1$tree)[3]))
  expect_silent(confint1(m1, sides = "right", digits = 2))
  expect_silent(confint1(m1, sides = "two.sided", var = names(m1$tree)[3:1]))
  expect_silent(confint1(m1, sides = "left", var = names(m1$tree)[3:1]))
})




