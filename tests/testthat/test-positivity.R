test_that("positivity finds the contexts missing a treatment value", {
  ## no crew member of either sex was a child
  viol <- positivity(full(Titanic, lambda = 0), "Age", "Survived")
  expect_s3_class(viol, "data.frame")
  expect_equal(nrow(viol), 2)
  expect_equal(viol$Class, c("Crew", "Crew"))
  expect_equal(viol$Age, c("Child", "Child"))
  expect_equal(viol$probability, c(0, 0))
  expect_named(viol, c("Class", "Sex", "Age", "probability"))
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

test_that("positivity skips contexts which cannot occur", {
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

  ## with X itself never taking a value, its contexts are not reported for TT
  d2 <- d[d$X == "0", ]
  d2$X <- factor(d2$X, levels = c("0", "1"))
  m2 <- full(d2, lambda = 0)
  expect_equal(nrow(positivity(m2, "TT", "Y")), 0)
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
