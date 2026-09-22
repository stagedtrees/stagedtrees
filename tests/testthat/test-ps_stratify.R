model <- stages_bhc(full(Titanic))

## $call records the call that produced the object, so two models built by
## different but equivalent calls differ only there
no_call <- function(x) {
  x$call <- NULL
  x
}

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
  expect_equal(no_call(model_ps_default), no_call(model_ps_explicit))
})

test_that("ps_stratify keeps the stages listed in ignore", {
  set.seed(3)
  D <- data.frame(
    X = sample(c("0", "1"), 300, TRUE),
    TT = sample(c("a", "b"), 300, TRUE),
    Y = sample(c("no", "yes"), 300, TRUE), stringsAsFactors = TRUE
  )
  # make (X = 1, TT = b) unobserved
  D <- D[!(D$X == "1" & D$TT == "b"), ]
  m <- join_unobserved(full(D, lambda = 0))
  expect_true(m$name_unobserved %in% m$stages$Y)

  m_ps <- ps_stratify(m, treatment = "TT", outcome = "Y")
  # the unobserved situation keeps its stage, so it is not pooled into
  # one of the new strata
  expect_equal(m_ps$stages$Y, c("1:a", "1:b", "2:a", m$name_unobserved))
  n_obs <- vapply(m_ps$prob$Y, function(p) attr(p, "n"), FUN.VALUE = 1)
  expect_equal(names(which(n_obs == 0)), m$name_unobserved)

  # ignore = NULL re-stages every situation
  m_ps0 <- ps_stratify(m, treatment = "TT", outcome = "Y", ignore = NULL)
  expect_equal(m_ps0$stages$Y, c("1:a", "1:b", "2:a", "2:b"))
})

test_that("ps_stratify is unaffected by ignore when nothing is unobserved", {
  expect_equal(
    no_call(ps_stratify(model, treatment = "Sex", outcome = "Age")),
    no_call(ps_stratify(model, treatment = "Sex", outcome = "Age", ignore = NULL))
  )
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

test_that("ps_stratify pairs a given variable with its neighbour", {
  ## supplying either one alone must give the same model
  expect_equal(
    no_call(ps_stratify(model, treatment = "Sex")),
    no_call(ps_stratify(model, treatment = "Sex", outcome = "Age"))
  )
  expect_equal(
    no_call(ps_stratify(model, outcome = "Age")),
    no_call(ps_stratify(model, treatment = "Sex", outcome = "Age"))
  )
  expect_error(ps_stratify(model, treatment = "Survived"), "last")
  expect_error(ps_stratify(model, outcome = "Class"), "first")
})

test_that("ps_stratify records the call which produced the object", {
  ps <- ps_stratify(model, treatment = "Sex", outcome = "Age")
  expect_equal(ps$call[[1]], as.name("ps_stratify"))
  ## and it replaces the call of the search which found the staging
  expect_equal(model$call[[1]], as.name("stages_bhc"))
  expect_output(print(summary(ps)), "ps_stratify")
  expect_output(print(ps), "ps_stratify")
})
