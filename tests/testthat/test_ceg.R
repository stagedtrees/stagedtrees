context("ceg")

model <- full(PhDArticles, lambda = 1)
model2 <- fbhc(model)

test_that("ceg function works", {
  expect_is(ceg(model), class = "ceg")
  expect_is(ceg(model2), class = "ceg")
})

test_that("ceg2adjmat", {
  expect_is(ceg2adjmat(ceg(model)), "matrix")
  expect_is(ceg2adjmat(ceg(model2)), "matrix")
})