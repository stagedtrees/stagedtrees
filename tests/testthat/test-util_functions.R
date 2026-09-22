test_that("new_lable produce a new label", {
  labels <- c(1:5, "6", "A", "B", "C")
  expect_false(stagedtrees:::new_label(labels) %in% labels)
})

test_that("uni_idx works as expected", {
  expect_silent(ui <-
    stagedtrees:::uni_idx(list(
      A = c(1, 2, 3), B = c(1, 2, 3)
    ), sep = ":"))
  expect_true(all(ui$A == c("A:1", "A:2", "A:3")))
})


test_that("default_treatment_outcome fills in the last two variables", {
  model <- full(Titanic)
  expect_equal(
    stagedtrees:::default_treatment_outcome(NULL, NULL, model),
    list(treatment = "Age", outcome = "Survived")
  )
  expect_equal(
    stagedtrees:::default_treatment_outcome("Sex", NULL, model),
    list(treatment = "Sex", outcome = "Survived")
  )
  expect_equal(
    stagedtrees:::default_treatment_outcome(NULL, "Class", model),
    list(treatment = "Age", outcome = "Class")
  )
})

test_that("default_treatment_outcome requires at least two variables", {
  m1 <- sevt_fit(
    sevt(list(A = c("a", "b")), full = TRUE),
    data.frame(A = c("a", "b", "a", "b")), lambda = 0
  )
  expect_error(stagedtrees:::default_treatment_outcome(NULL, NULL, m1))
})


test_that("which_class", {
  D <- factor("D", c("A", "B", "C", "D", "E"))
  A <- factor("A", c("A", "B", "C", "D", "E"))
  expect_equal(stagedtrees:::which_class(
    c(-Inf, -10, -4, -1, -6),
    c("A", "B", "C", "D", "E")
  ), !!D)
  expect_equal(stagedtrees:::which_class(
    c(-0.1, -10, -4, -1, -6),
    c("A", "B", "C", "D", "E")
  ), !!A)
})
