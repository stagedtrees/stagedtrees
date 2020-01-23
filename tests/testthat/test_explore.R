context("explore functions")

DD <- generate_linear_dataset(5, 100)[, 6:1]
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 0)
sev.lambda <- full(DD, lambda = 1)
sev.indep <- indep(DD, lambda = 0)

test_that("stages.sevt", {
  expect_equal(stages.sevt(sev, "X1"), as.character(1:32))
  expect_equal(stages.sevt(sev, "X3"), as.character(1:8))
})


test_that("summary.sevt for full model", {
  expect_visible(summary(sev))
  expect_visible(summary(sev.lambda))
  expect_silent(summary(sev))
  expect_silent(summary(sev.lambda))
})

test_that("summary.sevt for subtree", {
  expect_visible(summary(subtree.sevt(sev, c("a", "-1"))))
  expect_silent(summary(subtree.sevt(sev, c("a", "-1"))))
})

test_that("summary.sevt for indep", {
  expect_visible(summary(sev.indep))
  summary(summary(sev.indep))
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
