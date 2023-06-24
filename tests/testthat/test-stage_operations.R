DD <- generate_linear_dataset(5, 100)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 0, join_unobserved = FALSE)
sev.lambda <- full(DD, lambda = 1, join_unobserved = FALSE)
sev.indep <- indep(DD, lambda = 0, join_unobserved = FALSE)


test_that("stages function", {
  expect_equal(stages(sev, "X5"), as.character(1:32))
  expect_equal(stages(sev, "X3"), as.character(1:8))
})

test_that("rename_stage", {
  expect_s3_class(rename_stage(sev, var = "X2", 
                         stage = "2", new = "NEW"), "sevt")
})