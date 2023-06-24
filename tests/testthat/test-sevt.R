test_that("test that the creator works for list", {
  ev <-
    sevt(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_s3_class(ev, "sevt")
  ev <-
    sevt(x = list(
      c(1, 2),
      c("x", "y"),
      c("3", "4")
    ))
  expect_s3_class(ev, "sevt")
  expect_error(sevt(x = list(
    NULL,
    c("x", "y"),
    c("3", "4")
  )))
})

test_that("test that the creator works for data.frame", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
  ev <- sevt(x = DD, order = c("B", "A"))
  expect_s3_class(ev, "sevt")
})

test_that("test that the creator works for tables", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
  DD <- table(DD)
  ev <- sevt(x = DD, order = c("B", "A"))
  expect_s3_class(ev, "sevt")
})

test_that("test that the staged event tree is created with the right order I", {
  ev <-
    sevt(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_true(all(names(ev$tree) == c("A", "B", "C")))
})

test_that("test that the staged event tree is created with the right order II", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
  ev <- sevt(x = DD, order = c("B", "A"))
  expect_true(all(names(ev$tree) == c("B", "A")))
})
