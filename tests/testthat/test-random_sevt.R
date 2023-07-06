test_that("generate random sevt from list", {
  tree <- list(
    X = c("a", "b"), Y = c("c", "d", "e"),
    Z = c("1", "2", "3"), W = c("yes", "no")
  )
  model <- random_sevt(tree)

  expect_s3_class(model, "sevt")
  expect_equal(model$tree, tree)
})

test_that("generate random sevt from parentslist", {
  pl <- random_parentslist(11, k = 3, maxp = 2)
  mod <- random_sevt(pl)
  expect_s3_class(mod, "sevt")
})

test_that("generate random sevt from sevt", {
  start <- stages_bhc(full(Titanic))
  mod <- random_sevt(start)
  expect_s3_class(mod, "sevt")
  expect_equal(start$tree, mod$tree)
})

test_that("can sample data from generated sevt", {
  pl <- random_parentslist(11, k = 3, maxp = 2)
  mod <- random_sevt(pl)
  expect_s3_class(mod, "sevt")
  data <- sample_from(mod, size = 100)
  expect_s3_class(data, "data.frame")
  expect_equal(dim(data), c(100, 11))
  mod1 <- full(data)
  expect_equal(mod1$tree, mod$tree)
})
