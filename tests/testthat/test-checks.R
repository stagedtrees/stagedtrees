model <- full(PhDArticles)

test_that("has prob return TRUE for fitted model", {
  expect_true(has_prob(model))
})

test_that("has prob return FALSE if no prob field", {
  model_ <- model
  model_$prob <- NULL
  expect_false(has_prob(model_))
})

test_that("has prob return FALSE if one var missing from probabilities", {
  model_ <- model
  model_$prob$Kids <- NULL
  expect_false(has_prob(model_))
})


test_that("has prob return FALSE if one var has wrong length prob", {
  model_ <- model
  model_$prob$Kids[["4"]] <- c(0.1, 0.4, 0.5)
  expect_false(has_prob(model_))
})
