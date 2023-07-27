test_that("stages_csbhc works", {
  start <- random_sevt(list(A = c("a", "aa", "aaa"),
                            B = c("b", "bb"),
                            C = c("c", "cc")))
  start <- sevt_fit(start, sample_from(start, 1000))
  expect_silent(res <- stages_csbhc(start))
})
