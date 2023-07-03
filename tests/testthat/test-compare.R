DD <- generate_linear_dataset(5, 10)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")

mod1 <- full(DD, join_unobserved = FALSE)
mod2 <- indep(DD, join_unobserved = FALSE)

test_that("compare_stages correctly returns TRUE/FALSE", {
  if (!requireNamespace("clue", quietly = TRUE)) {
    methods <- c("naive", "stages")
  } else {
    methods <- c("naive", "hamming", "stages")
  }


  for (m in methods) {
    expect_true(compare_stages(mod1, mod1, method = !!m))
    expect_true(compare_stages(mod2, mod2, method = !!m))
    expect_false(compare_stages(mod1, mod2, method = !!m))
    expect_false(compare_stages(mod2, mod1, method = !!m))
  }
})

test_that("hamming_stages", {
  expect_true(hamming_stages(mod1, mod2) >= 0)
})
