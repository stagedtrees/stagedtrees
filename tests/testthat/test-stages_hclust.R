test_that("stages_hclust works when a variable has exactly 1 non-ignored stage (B-B3)", {
  m <- indep(data.frame(
    X = factor(c("a", "b")),
    Y = factor(c("1", "2"))
  ), lambda = 1)
  # Y has exactly 1 unique stage in the independence model
  for (dist in c("totvar", "hellinger", "euclidean", "manhattan")) {
    expect_no_error(stages_hclust(m, distance = dist, k = 1))
  }
  expect_s3_class(stages_hclust(m, k = 1), "sevt")
})
