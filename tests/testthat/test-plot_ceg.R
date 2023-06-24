mod <- random_sevt(list(
  "A" = c("aa", "aaa", "a"),
  "B" = c("b", "bbb"),
  "C" = c("c", "cc")
))

test_that("plot.ceg", {
  expect_silent(plot(ceg(mod)))
})

test_that("plot.ceg", {
  expect_silent(plot(ceg(mod), col = "stages"))
})
