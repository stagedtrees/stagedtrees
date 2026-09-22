test_that("positivity finds the contexts missing a treatment value", {
  ## no crew member of either sex was a child
  viol <- positivity(full(Titanic, lambda = 0), "Age", "Survived")
  expect_s3_class(viol, "data.frame")
  expect_equal(nrow(viol), 2)
  expect_equal(viol$Class, c("Crew", "Crew"))
  expect_equal(viol$Age, c("Child", "Child"))
  expect_true(all(viol$context_probability > 0))
  expect_named(viol, c("Class", "Sex", "Age", "context_probability"))
})

test_that("positivity reports nothing when the assumption holds", {
  expect_equal(nrow(positivity(full(Titanic, lambda = 0), "Sex", "Survived")), 0)
  ## a prior gives every value positive probability, repairing the violation
  expect_equal(nrow(positivity(full(Titanic, lambda = 1), "Age", "Survived")), 0)
})

test_that("positivity is checked on the probabilities it is given", {
  ## staging borrows probability from the other contexts of the stage, so a
  ## violation of the data need not be one of the staged model
  m <- full(Titanic, lambda = 0)
  expect_equal(nrow(positivity(m, "Age", "Survived")), 2)
  stages(m)["Age"] <- "same"
  expect_equal(nrow(positivity(m, "Age", "Survived")), 0)
})

test_that("positivity reports a context which never receives a value", {
  set.seed(3)
  d <- data.frame(
    X = factor(sample(c("0", "1"), 300, TRUE)),
    TT = factor(sample(c("a", "b"), 300, TRUE)),
    Y = factor(sample(c("no", "yes"), 300, TRUE))
  )
  d <- d[!(d$X == "1" & d$TT == "b"), ]
  m <- full(d, lambda = 0)
  viol <- positivity(m, "TT", "Y")
  expect_equal(viol$X, "1")
  expect_equal(viol$TT, "b")
  expect_gt(viol$context_probability, 0)
})

test_that("a context which does not occur is reported with probability zero", {
  set.seed(7)
  d <- data.frame(
    A = factor(sample(c("a1", "a2"), 400, TRUE)),
    B = factor(sample(c("b1", "b2"), 400, TRUE)),
    TT = factor(sample(c("t1", "t2"), 400, TRUE)),
    Y = factor(sample(c("n", "y"), 400, TRUE))
  )
  d <- d[!(d$A == "a2" & d$B == "b2"), ]   # this context never occurs
  m <- full(d, lambda = 0)

  ## such a context sits in the unobserved stage, which ignore leaves out
  expect_equal(nrow(positivity(m, "TT", "Y")), 0)
  expect_equal(attr(positivity(m, "TT", "Y"), "n_ignored"), 1)

  ## the model has no probabilities at all there, so every value of the
  ## treatment is unattainable: one row for the context, listing them
  viol <- positivity(m, "TT", "Y", ignore = NULL)
  expect_equal(nrow(viol), 1)
  expect_equal(viol$A, "a2")
  expect_equal(viol$B, "b2")
  expect_equal(viol$TT, "t1, t2")
  expect_equal(viol$context_probability, 0)
  expect_equal(attr(viol, "n_ignored"), 0)

  ## a staging which covers the context gives it probabilities, and then
  ## there is nothing left for positivity to report on this model
  stages(m)["TT"] <- "pooled"
  expect_equal(nrow(positivity(m, "TT", "Y")), 0)
})

test_that("positivity says how many contexts ignore left out", {
  set.seed(7)
  d <- data.frame(
    A = factor(sample(c("a1", "a2"), 600, TRUE)),
    B = factor(sample(c("b1", "b2", "b3"), 600, TRUE)),
    TT = factor(sample(c("t1", "t2"), 600, TRUE)),
    Y = factor(sample(c("n", "y"), 600, TRUE))
  )
  m <- full(d[!(d$A == "a2" & d$B %in% c("b2", "b3")), ], lambda = 0)
  expect_equal(attr(positivity(m, "TT", "Y"), "n_ignored"), 2)
  expect_output(print(positivity(m, "TT", "Y")), "No positivity violations")
  expect_message(print(positivity(m, "TT", "Y")), "ignore = NULL")
  expect_message(print(positivity(m, "TT", "Y")), "2 contexts")
  ## nothing to report when they are all shown
  expect_no_message(print(positivity(m, "TT", "Y", ignore = NULL)))
})

test_that("positivity works when treatment is the first variable", {
  m <- full(Titanic, lambda = 0)
  expect_equal(nrow(positivity(m, "Class", "Survived")), 0)
})

test_that("positivity checks its arguments", {
  m <- full(Titanic, lambda = 0)
  expect_error(positivity(m, "Survived", "Age"))  # outcome before treatment
  expect_error(positivity(m, "Age", "Age"))       # outcome is the treatment
  expect_error(positivity(m, "NotAVar", "Survived"))
  expect_error(positivity(sevt(list(A = c("a", "b"), B = c("x", "y")),
                               full = TRUE), "A", "B"))  # no probabilities
})

test_that("positivity finds a violation of the first variable", {
  ## the single stage of the first variable is stored under its own name and
  ## not under the "NA" that stages() reports, so looking it up by that name
  ## silently found nothing and every such context passed as fine
  set.seed(1)
  d <- data.frame(
    A = factor(sample(c("a1", "a2"), 200, TRUE), levels = c("a1", "a2", "a3")),
    Y = factor(sample(c("n", "y"), 200, TRUE))
  )
  m <- full(d, lambda = 0)
  expect_equal(unname(m$prob$A[[1]][["a3"]]), 0)
  viol <- positivity(m, "A", "Y")
  expect_equal(nrow(viol), 1)
  expect_equal(viol$A, "a3")
  expect_equal(viol$context_probability, 1)
})

test_that("positivity keeps a context variable named like the treatment column", {
  set.seed(1)
  d <- data.frame(
    treatment = factor(sample(c("a1", "a2"), 400, TRUE)),
    TT = factor(sample(c("t1", "t2"), 400, TRUE)),
    Y = factor(sample(c("n", "y"), 400, TRUE))
  )
  d <- d[!(d$treatment == "a2" & d$TT == "t2"), ]
  viol <- positivity(full(d, lambda = 0), "TT", "Y")
  expect_named(viol, c("treatment", "TT", "context_probability"))
  expect_equal(viol$treatment, "a2")
  expect_equal(viol$TT, "t2")
})
