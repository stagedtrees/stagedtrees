DD <- generate_linear_dataset(5, 100)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 1)

test_that("predict class values", {
  expect_silent(pr <- predict(sev, DD))
  expect_true(all(levels(pr) == levels(DD$C)))
  expect_silent(pr <- predict(sev, class = "X3", DD))
  expect_true(all(levels(pr) == levels(DD$X3)))
})


test_that("predict probabilities", {
  pr <- predict(sev, DD, prob = TRUE, log = FALSE)
  expect_true(all(pr >= 0))
  pr <- predict(sev, DD, class = "X4", prob = TRUE, log = FALSE)
  expect_true(all(pr >= 0))
})


test_that("predict log-probabilities", {
  expect_silent(pr <- predict(sev, DD, prob = TRUE, log = TRUE))
  expect_silent(pr <- predict(sev, DD, class = "X4", prob = TRUE, log = TRUE))
})


test_that("predict with no data", {
  expect_silent(pr <- predict(sev, class = "X4"))
})


test_that("predict throws errors", {
  expect_error(predict(sev, class = "CC"))
  sev$ctables <- NULL
  expect_error(predict(sev))
  sev$prob <- NULL
  expect_error(predict(sev))
})
