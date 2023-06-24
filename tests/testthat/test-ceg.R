model <- full(PhDArticles, lambda = 1)
model2 <- stages_fbhc(model)

test_that("ceg function works", {
  expect_s3_class(ceg(model), class = "ceg")
  expect_s3_class(ceg(model2), class = "ceg")
})

test_that("ceg2adjmat", {
  expect_type(as_adj_matrix(ceg(model)), "double")
  expect_type(as_adj_matrix(ceg(model2)), "double")
})


mod <- random_sevt(list(
  "A" = c("aa", "aaa", "a"),
  "B" = c("b", "bbb")
))

test_that("2 var ceg", {
  expect_silent(ceg(mod))
})

mod <- random_sevt(list(
  "A" = c("aa", "aaa", "a")
))

test_that("1 var ceg", {
  expect_silent(ceg(mod))
})
