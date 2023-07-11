DD <- generate_linear_dataset(5, 100)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 1)

test_that("probabilities are ok", {
  pr <- prob(sev, c(X1 = "-1", X3 = "pppp"))
  expect_true(all(pr >= 0))
  pr <- prob(sev, c(C = "a"))
  expect_true(all(pr >= 0))
  pr <- prob(sev, c(X1 = "1"))
  expect_true(all(pr >= 0))
  pr <- prob(sev, c(C = "a"))
  expect_true(all(abs(pr - sev$prob$C$`1`["a"]) < 1e-10))
})

test_that("probabilities are positive", {
  pr <- prob(sev, DD)
  expect_true(all(pr >= 0))
})

test_that("conditional probabilities are positive", {
  pr <- prob(sev, DD[, c(1, 3, 4)], conditional_on = DD[, c(2, 5)])
  expect_true(all(pr >= 0))
})

test_that("conditional probabilities are positive", {
  pr <- prob(sev, DD[, c(1, 3, 4)], conditional_on = c(X1 = "1"))
  expect_true(all(pr >= 0))
})


test_that("probabilities sum to 1", {
  pr <- prob(sev, expand.grid(sev$tree))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
  pr <- prob(sev, expand.grid(sev$tree[1:3]))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
})

test_that("conditional probabilities sum to 1", {
  pr <- prob(sev, expand.grid(sev$tree))
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
  pr <- prob(sev, expand.grid(sev$tree[1:3]),
    conditional_on = c(X3 = "pppp", X5 = "-1")
  )
  expect_true(all(abs(sum(pr) - 1) < 1e-10))
})

test_that("probability for object with wrong order in $prob", {
  pr <- prob(sev, c(X4 = "-1"))
  sev$prob <- lapply(sev$prob, function(pp) pp[sample(seq_along(pp))])
  sev$prob <- sev$prob[sample(seq_along(sev$prob))]
  expect_equal(pr, prob(sev, c(X4 = "-1")))
})

test_that("probability for object with wrong order in $stages", {
  pr <- prob(sev, c(X4 = "-1"))
  sev$stages <- sev$stages[sample(seq_along(sev$stages))]
  expect_equal(pr, prob(sev, c(X4 = "-1")))
})

test_that("prob should raise error if x and conditional_on has same names", {
  ### this first check is just to check that an error is not from something else
  expect_length(prob(sev, c(X4 = "-1"), conditional_on = c(X1 = "1")), 1)
  expect_length(prob(sev, c(X4 = "-1")), 1)
  ## now we test what we want
  expect_error(prob(sev, c(X4 = "-1"), conditional_on = c(X1 = "1", X4 = "1")))
  expect_error(prob(sev, data.frame(X4 = c("-1", "1")), conditional_on = c(X1 = "1", X4 = "1")))
})

test_that("prob should raise error if x and conditional_on has same names", {
  ### this first check is just to check that an error is not from something else
  expect_length(prob(sev, c(X4 = "-1"), conditional_on = c(X1 = "1")), 1)
  expect_length(prob(sev, c(X4 = "-1")), 1)
  ## now we test what we want
  con <- data.frame(X1 = c("1" , "1"), X4 = c("1", "1"))
  expect_error(prob(sev, c(X4 = "-1"), conditional_on = con))
  expect_error(prob(sev, data.frame(X4 = c("-1", "1")), conditional_on = con))
})

test_that("prob should raise error conditional_on is not data.frame, vector or NULL", {
  ### this first check is just to check that an error is not from something else
  expect_length(prob(sev, c(X4 = "-1"), conditional_on = c(X1 = "1")), 1)
  expect_length(prob(sev, c(X4 = "-1")), 1)
  expect_length(prob(sev, c(X4 = "-1"), conditional_on = NULL), 1)
  expect_length(prob(sev, data.frame(X4 = "-1"),
                     conditional_on = data.frame(X1 = c("1", "1"))), 2)
  ## now we test what we want
  expect_error(prob(sev, c(X4 = "-1"), conditional_on = "1"))
  expect_error(prob(sev, data.frame(X4 = c("-1", "1")), conditional_on = 1))
})
