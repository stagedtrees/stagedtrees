context("compare.sevt")

test_that("compare correctly returns TRUE/FALSE", {
  DD <- generate_linear_dataset(5, 10)
  levels(DD$C) <- c("a", "b")
  levels(DD$X3) <- c("qqqq", "pppp")
  
  mod1 <- full(DD)
  mod2 <- indep(DD)
  methods <- c("naive", "hamming", "stages")
  
  for ( m in methods){
    expect_true(compare.sevt(mod1, mod1, method = m))
    expect_true(compare.sevt(mod2, mod2, method = m))
    expect_false(compare.sevt(mod1, mod2, method = m))
    expect_false(compare.sevt(mod2, mod1, method = m))
  }
})
