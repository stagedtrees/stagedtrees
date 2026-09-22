model0 <- model1 <- sevt(list(X1 = c("0", "1"),
                    X2 = c("0", "1"),
                    TT = c("a", "b"),
                    Y = c("-1", '1')), full = TRUE)

stages(model0)[["TT"]] <- c("1", "2", "2", "1")
stages(model0)[["Y"]] <- c("1", "1", "1", "2", "3", "2", "3", "3")



test_that("potential_outcomes return probabilities", {
  for (i in 1:10){
    model <- random_sevt(model0, q = 0)
    expect_silent(po <- potential_outcomes(model, outcome = "Y", treatment = "TT"))
    expect_lt(sum(abs(rowSums(po) - 1)), 1e-14)
  }
})


test_that("potential_outcomes return probabilities", {
  for (i in 1:10){
    model <- random_sevt(model1, q = 0.3)
    expect_silent(po <- potential_outcomes(model, outcome = "Y", treatment = "TT"))
    expect_lt(sum(abs(rowSums(po) - 1)), 1e-14)
  }
})

test_that("potential_outcomes defaults to the last two variables", {
  model <- random_sevt(model0, q = 0)
  po_default <- potential_outcomes(model)
  po_explicit <- potential_outcomes(model, treatment = "TT", outcome = "Y")
  expect_equal(po_default, po_explicit)
})

test_that("potential_outcomes defaults need at least two variables", {
  m1 <- sevt_fit(
    sevt(list(A = c("a", "b")), full = TRUE),
    data.frame(A = c("a", "b", "a", "b")), lambda = 0
  )
  expect_error(potential_outcomes(m1))
})

test_that("randomize_sevt records the call which produced the object", {
  m <- stages_bhc(full(Titanic))
  r <- randomize_sevt(m, "Age")
  expect_equal(r$call[[1]], as.name("randomize_sevt"))
  expect_output(print(r), "randomize_sevt")
})

test_that("randomize_sevt marks the randomized probabilities as estimated from nothing", {
  m <- stages_bhc(full(Titanic))
  r <- randomize_sevt(m, "Age")
  expect_true(is.na(attr(r$prob$Age[["randomized"]], "n", exact = TRUE)))
})

test_that("randomize_sevt leaves the stages of a first-variable treatment alone", {
  ## the first variable has no stages entry, and assigning into it made one
  ## of length zero
  set.seed(1)
  d <- data.frame(A = factor(sample(c("a", "b"), 200, TRUE)),
                  B = factor(sample(c("x", "y"), 200, TRUE)))
  m <- full(d, lambda = 0)
  r <- randomize_sevt(m, "A")
  expect_equal(stages(r)[["A"]], stages(m)[["A"]])
  expect_equal(as.vector(r$prob$A[[1]]), c(0.5, 0.5))
  expect_true(is.na(attr(r$prob$A[[1]], "n", exact = TRUE)))
  ## and the potential outcomes are still the randomized ones
  po <- potential_outcomes(r, treatment = "A", outcome = "B")
  expect_equal(unname(rowSums(po)), c(1, 1))
})
