
tree <- list(A = c("a", "aa"),
             B = c("b", "bb", "bbb"),
             C = c("c", "cc", "ccc"),
             D = c("d", "dd"))

test_that("tree_string", {
  expect_silent(ts <- tree_string(tree, max = 10))
})

test_that("tree_string", {
  expect_silent(ts <- tree_string(tree, max = 2))
  expect_equal(ts, "A[2]->B[3]->... [2 omitted]")
})
