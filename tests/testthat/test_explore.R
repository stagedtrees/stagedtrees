context("explore functions")

DD <- generate_linear_dataset(5, 100)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 0)
sev.lambda <- full(DD, lambda = 1)
sev.indep <- indep(DD, lambda = 0)

test_that("stages function", {
  expect_equal(stages(sev, "X5"), as.character(1:32))
  expect_equal(stages(sev, "X3"), as.character(1:8))
})


test_that("summary.sevt for full model", {
  expect_visible(summary(sev))
  expect_visible(summary(sev.lambda))
  expect_silent(summary(sev))
  expect_silent(summary(sev.lambda))
})

test_that("summary.sevt for subtree", {
  expect_visible(summary(subtree(sev, c("a", "-1"))))
  expect_silent(summary(subtree(sev, c("a", "-1"))))
})

test_that("summary.sevt for indep", {
  expect_visible(summary(sev.indep))
  summary(summary(sev.indep))
})


test_that("subtree", {
  expect_silent(sub <- subtree(sev, c("a", "1", "-1")))
  expect_silent(plot(sub))
  expect_silent(stages_fbhc(sub))
  expect_silent(stagedtrees:::sevt_fit(sub, data = DD))
})

test_that("sevt_df", {
  expect_equal(stagedtrees:::sevt_df(sev), 63)
})


test_that("print.sevt", {
  expect_output(print(sev))
})

test_that("summary.sevt is printed", {
  expect_output(print(summary(sev)))
})


test_that("rename_stage", {
  expect_is(rename_stage(sev, var = "X2", 
                         stage = "2", new.label = "NEW"), "sevt")
})
