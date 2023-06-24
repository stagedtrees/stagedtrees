sev <- random_sevt(list(A = c("aa", "aaa"),
                        B = c("b", "bb", "bbb")))

test_that("print.sevt", {
  expect_output(print(sev))
})
