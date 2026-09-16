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

test_that("prob: conditioning on zero-prob event returns NA with warning, not 1 (C2)", {
  data(Titanic)
  # lambda=0 so P(Crew, Child) = 0 (no crew children in Titanic data)
  m <- full(Titanic, lambda = 0)
  cond <- c(Class = "Crew", Age = "Child")
  expect_equal(prob(m, cond), 0)  # joint query still returns 0

  # conditional on impossible event -> NA + warning, not 1
  expect_warning(
    p_yes <- prob(m, c(Survived = "Yes"), conditional_on = cond, na0 = FALSE),
    regexp = "zero-probability"
  )
  expect_true(is.na(p_yes))

  expect_warning(
    p_no <- prob(m, c(Survived = "No"), conditional_on = cond, na0 = FALSE),
    regexp = "zero-probability"
  )
  expect_true(is.na(p_no))

  # with na0 = TRUE (default) zero-prob conditioning is STILL NA (not 0 or 1)
  expect_warning(
    p_default <- prob(m, c(Survived = "Yes"), conditional_on = cond),
    regexp = "zero-probability"
  )
  expect_true(is.na(p_default))

  # vectorised data.frame conditioning: only impossible rows become NA
  cond_df <- data.frame(Class = c("Crew", "1st"), Age = c("Child", "Adult"))
  expect_warning(
    pv <- prob(m, data.frame(Survived = c("Yes", "Yes")),
               conditional_on = cond_df, na0 = FALSE),
    regexp = "zero-probability"
  )
  expect_true(is.na(pv[1]))
  expect_false(is.na(pv[2]))
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

test_that("prob: 0-row data frame returns numeric(0), not an error (B-B4)", {
  m <- full(Titanic, lambda = 1)
  empty <- data.frame(Class = character(0), Survived = character(0))
  expect_no_error(prob(m, empty))
  expect_equal(length(prob(m, empty)), 0L)
})

## prob() now reads the query through a character matrix built once, and takes
## a short path for rows with no missing values. Two behaviours distinguish
## that path from the general one and are pinned here.

test_that("a model variable absent from x is treated as unobserved", {
  ## x[i, vv] returns NULL for a column a data.frame does not have, which the
  ## row loop read as "sum over every level of that variable". Leaving a gap
  ## in the middle of the variable order must therefore marginalise it, not
  ## error and not silently drop it.
  set.seed(21)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 400, TRUE)),
    B = factor(sample(c("x", "y"), 400, TRUE)),
    C = factor(sample(c("p", "q"), 400, TRUE))
  )
  m <- full(d, lambda = 1)
  gap <- d[1:5, c("A", "C")]
  ## marginalising B by omission equals summing over its levels explicitly
  explicit <- vapply(seq_len(5), function(i) {
    sum(vapply(levels(d$B), function(b) {
      prob(m, data.frame(A = d$A[i], B = factor(b, levels(d$B)), C = d$C[i]))
    }, 1.0))
  }, 1.0)
  expect_equal(as.vector(prob(m, gap)), explicit)
})

test_that("a fully observed path through an unobserved stage reduces to -Inf", {
  ## The row loop reduces each row with logSumExp(..., na.rm = TRUE). An
  ## unobserved situation carries NA probabilities, so path_probability()
  ## returns NA for a path through it, and na.rm turns that into -Inf. A short
  ## path returning the log-probability directly would yield NA instead, which
  ## na0 = FALSE would then preserve all the way out.
  set.seed(23)
  n <- 300
  d <- data.frame(
    A = factor(sample(c("a", "b"), n, TRUE)),
    B = factor(sample(c("x", "y"), n, TRUE)),
    C = factor(sample(c("p", "q"), n, TRUE))
  )
  d <- d[!(d$A == "a" & d$B == "y"), ]   # situation (a, y) never occurs
  m <- full(d, lambda = 0, join_unobserved = TRUE)
  expect_true(m$name_unobserved %in% as.character(stages(m)$C))
  expect_true(all(is.na(m$prob$C[[m$name_unobserved]])))

  q <- data.frame(A = factor("a", c("a", "b")),
                  B = factor("y", c("x", "y")),
                  C = factor("p", c("p", "q")))
  ## the path itself is NA ...
  expect_true(is.na(stagedtrees:::path_probability(m, c("a", "y", "p"), log = TRUE)))
  ## ... but prob() reports -Inf on the log scale and 0 on the natural one,
  ## whether or not na0 is asked to convert NAs
  expect_equal(as.vector(prob(m, q, log = TRUE, na0 = FALSE)), -Inf)
  expect_equal(as.vector(prob(m, q, log = TRUE, na0 = TRUE)), -Inf)
  for (n0 in c(TRUE, FALSE)) {
    got <- prob(m, q, na0 = n0)
    expect_false(is.na(got))
    expect_equal(as.vector(got), 0)
  }
})
