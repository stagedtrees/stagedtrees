test_that("generate random parentslist", {
  pl <- random_parentslist(23, k = 3, maxp = 7)
  
  expect_s3_class(pl, "parentslist")
  expect_length(pl, 23)
  for (i in 1:23) {
    expect_lte(length(pl[[i]]$parents), 7)
    expect_lte(length(pl[[i]]$values), 3)
  }
})