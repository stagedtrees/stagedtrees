test_that("noisy_xor works as expected", {
  expect_true(noisy_xor(c(-1, -1), 0) == 1)
  expect_true(noisy_xor(c(-1, -1, -1), 0) == -1)
  expect_true(noisy_xor(c(-1, 1), 0) == -1)
  expect_true(noisy_xor(c(1, -1), 0) == -1)
})

test_that("generate_xor_dataset", {
  DD <- generate_xor_dataset(p = 5, n = 99)
  expect_equal(dim(DD), c(99, 6))
  expect_equal(colnames(DD)[1], "C")
})

test_that("generate_linear_dataset", {
  DD <- generate_linear_dataset(p = 5, n = 99)
  expect_equal(dim(DD), c(99, 6))
  expect_equal(colnames(DD)[1], "C")
})

test_that("generate_random_dataset", {
  DD <- generate_random_dataset(p = 5, n = 99)
  expect_equal(dim(DD), c(99, 5))
})
