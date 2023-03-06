context("ceg")

model <- full(PhDArticles, lambda = 1)
model2 <- stages_fbhc(model)

test_that("ceg function works", {
  expect_is(ceg(model), class = "ceg")
  expect_is(ceg(model2), class = "ceg")
})

test_that("ceg2adjmat", {
  expect_is(as_adj_matrix(ceg(model)), "matrix")
  expect_is(as_adj_matrix(ceg(model2)), "matrix")
})