context("explore functions")

DD <- generate_linear_dataset(5, 100)[,6:1]
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 1)

test_that("stages.sevt", {
  expect_equal(stages.sevt(sev, "X1"), as.character(1:32)) 
  expect_equal(stages.sevt(sev, "X3"), as.character(1:8)) 
})


test_that("summary.sevt", {
  expect_visible(summary(sev))
})


test_that("subtree.sevt", {
  expect_silent(sub <- subtree.sevt(sev, c("a", "1", "-1")))
  expect_silent(plot(sub))
  expect_silent(fbhc.sevt(sub))
  expect_silent(fit.sevt(sub, data = DD))
#  expect_true(BIC(sub2) <= BIC(sub))
#  expect_true(logLik(sub2) <= logLik(sub))
})

test_that("df.sevt", {
  expect_equal(df.sevt(sev), 63)
})
