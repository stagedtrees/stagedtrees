DD <- generate_linear_dataset(5, 100)
DDna <- DD
DDna[sample(100, 10), sample(6, 1)] <- NA
DDna[sample(100, 10), sample(6, 1)] <- NA
DDna[sample(100, 10), sample(6, 1)] <- NA
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, order = c("X1", "C", "X2"))


## not testing with stages_hc because slow
algs <- list(
  stages_bhc,
  stages_bj,
  stages_fbhc
)

test_that("search_best for data.frames", {
  expect_s3_class(search_best(data = DD, alg = stages_fbhc), class = "sevt")
})

test_that("search_best for data.frames with NAs", {
  expect_s3_class(search_best(data = DD, alg = stages_fbhc), class = "sevt")
})

DD.t <- table(DD)
test_that("search_best for table", {
  expect_s3_class(search_best(data = DD.t, alg = stages_fbhc), class = "sevt")
})
