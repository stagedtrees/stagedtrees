model <- stages_bhc(full(Titanic))

test_that("ps_stratify builds one stage per treatment-stage/value pair", {
  model_ps <- ps_stratify(model, treatment = "Sex", outcome = "Age")
  expect_true(is_fitted_sevt(model_ps))
  lv <- model$tree[["Sex"]]
  st <- model$stages[["Sex"]]
  expect_equal(
    stages(model_ps)[["Age"]],
    paste(rep(st, each = length(lv)), rep(lv, times = length(st)), sep = ":")
  )
  # unrelated staging is untouched
  expect_equal(model_ps$stages$Sex, model$stages$Sex)
  expect_equal(stages(model_ps)[["Class"]], stages(model)[["Class"]])
  expect_equal(model_ps$tree, model$tree)
})

test_that("ps_stratify defaults to the last two variables", {
  model_ps_default <- ps_stratify(model)
  model_ps_explicit <- ps_stratify(model, treatment = "Age", outcome = "Survived")
  expect_equal(model_ps_default, model_ps_explicit)
})

test_that("ps_stratify works when treatment is the first variable", {
  # the first variable has a NULL entry in object$stages, only the
  # stages() accessor reports its (single) stage
  model_ps <- ps_stratify(model, treatment = "Class", outcome = "Sex")
  expect_true(is_fitted_sevt(model_ps))
  expect_equal(
    stages(model_ps)[["Sex"]],
    paste("NA", model$tree[["Class"]], sep = ":")
  )
  expect_false(any(!is.finite(unlist(model_ps$prob$Sex))))
})

test_that("ps_stratify defaults need at least two variables", {
  m1 <- sevt_fit(
    sevt(list(A = c("a", "b")), full = TRUE),
    data.frame(A = c("a", "b", "a", "b")), lambda = 0
  )
  expect_error(ps_stratify(m1))
})

test_that("ps_stratify requires a fitted object", {
  model_unfit <- sevt(list(
    Class = c("1st", "2nd", "3rd", "Crew"),
    Sex = c("Male", "Female"),
    Age = c("Child", "Adult"),
    Survived = c("No", "Yes")
  ), full = TRUE)
  expect_error(ps_stratify(model_unfit, treatment = "Sex", outcome = "Age"))
})

test_that("ps_stratify requires outcome to immediately follow treatment", {
  expect_error(ps_stratify(model, treatment = "Class", outcome = "Survived"))
})

test_that("ps_stratify checks that variables are in scope", {
  expect_error(ps_stratify(model, treatment = "Sex", outcome = "NotAVar"))
  expect_error(ps_stratify(model, treatment = "NotAVar", outcome = "Age"))
})
