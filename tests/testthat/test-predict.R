DD <- generate_linear_dataset(5, 100)
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, lambda = 1)

test_that("predict class values", {
  expect_silent(pr <- predict(sev, DD))
  expect_true(all(levels(pr) == levels(DD$C)))
  expect_silent(pr <- predict(sev, class = "X3", DD))
  expect_true(all(levels(pr) == levels(DD$X3)))
})


test_that("predict probabilities", {
  pr <- predict(sev, DD, prob = TRUE, log = FALSE)
  expect_true(all(pr >= 0))
  pr <- predict(sev, DD, class = "X4", prob = TRUE, log = FALSE)
  expect_true(all(pr >= 0))
})


test_that("predict log-probabilities", {
  expect_silent(pr <- predict(sev, DD, prob = TRUE, log = TRUE))
  expect_silent(pr <- predict(sev, DD, class = "X4", prob = TRUE, log = TRUE))
})


test_that("predict with no data", {
  expect_silent(pr <- predict(sev, class = "X4"))
})


test_that("predict throws errors", {
  expect_error(predict(sev, class = "CC"))
  sev$ctables <- NULL
  expect_error(predict(sev))
  sev$prob <- NULL
  expect_error(predict(sev))
})

## Rows whose predictors are all present are handled by a compiled kernel, the
## rest by the original prob()-based path. The risks are that the two paths
## disagree, that rows get mis-assigned when a batch mixes them, and that the
## reshaping of the result differs.

test_that("the compiled path agrees with computing each path in R", {
  set.seed(41)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 500, TRUE)),
    B = factor(sample(c("x", "y", "z"), 500, TRUE)),
    C = factor(sample(c("p", "q"), 500, TRUE)),
    D = factor(sample(c("u", "v", "w"), 500, TRUE))
  )
  n <- 0
  for (m in list(full(d, lambda = 1), stages_bhc(full(d, lambda = 1)))) {
    vs <- sevt_varnames(m)
    q <- d[1:25, vs]
    for (cl in vs) {                       # class at every position
      got <- predict(m, newdata = q, class = cl, prob = TRUE, log = TRUE)
      ## oracle: walk each candidate path in R, then normalise as predict does
      want <- t(vapply(seq_len(nrow(q)), function(i) {
        x <- as.character(unlist(q[i, vs]))
        names(x) <- vs
        res <- vapply(m$tree[[cl]], function(cv) {
          x[cl] <- cv
          stagedtrees:::path_probability(m, x, log = TRUE)
        }, 1.0)
        res[is.nan(res)] <- -Inf
        res - log(sum(exp(res)))
      }, numeric(length(m$tree[[cl]]))))
      expect_equal(unname(got), unname(want))
      n <- n + 1
    }
  }
  expect_gt(n, 0)
})

test_that("rows with a missing predictor are marginalised, not fed to the kernel", {
  ## The batch is split into a compiled group and a prob() group. The oracle
  ## here is prob() directly, NOT predict() on one row at a time: with a
  ## dispatch bug both sides of that comparison would be wrong together, and
  ## it would pass.
  set.seed(42)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 400, TRUE)),
    B = factor(sample(c("x", "y"), 400, TRUE)),
    C = factor(sample(c("p", "q"), 400, TRUE))
  )
  m <- full(d, lambda = 1)
  vs <- sevt_varnames(m)
  q <- d[1:12, vs]
  na_rows <- c(2L, 5L, 9L)
  q[na_rows, "B"] <- NA              # these need the sum over B's levels
  got <- predict(m, newdata = q, class = "A", prob = TRUE, log = TRUE)

  want <- t(vapply(seq_len(nrow(q)), function(i) {
    x <- unlist(lapply(q[i, vs], as.character))
    names(x) <- vs
    res <- vapply(m$tree$A, function(cv) {
      x[["A"]] <- cv
      prob(m, as.data.frame(t(x[!is.na(x)]), stringsAsFactors = FALSE),
           log = TRUE)
    }, 1.0)
    res[is.nan(res)] <- -Inf
    res - log(sum(exp(res)))
  }, numeric(length(m$tree$A))))

  expect_equal(unname(got), unname(want))
  expect_false(any(is.na(got)))
  ## the rows carrying NA really did take a different route: their answer is
  ## not the one the kernel would give for any single completion of B
  for (i in na_rows) {
    completions <- vapply(levels(d$B), function(b) {
      qq <- q[i, , drop = FALSE]; qq$B <- factor(b, levels(d$B))
      predict(m, newdata = qq, class = "A", prob = TRUE, log = TRUE)[1, 1]
    }, 1.0)
    expect_false(any(abs(completions - got[i, 1]) < 1e-12))
  }
})

test_that("a class with a single level gives one prediction per observation", {
  ## apply() returns a vector rather than a matrix when the class has one
  ## level, so t() used to hand back a 1 x n matrix and prob = FALSE collapsed
  ## to a single value for the whole of newdata
  set.seed(43)
  d <- data.frame(
    A = factor(rep("a", 120)),
    B = factor(sample(c("x", "y"), 120, TRUE)),
    C = factor(sample(c("p", "q"), 120, TRUE))
  )
  m <- full(d, lambda = 1)
  q <- d[1:10, ]
  pm <- predict(m, newdata = q, class = "A", prob = TRUE, log = TRUE)
  expect_equal(dim(pm), c(10L, 1L))
  expect_equal(colnames(pm), "a")
  expect_true(all(pm == 0))          # only one level, so it has all the mass
  cl <- predict(m, newdata = q, class = "A")
  expect_length(cl, 10L)
  expect_identical(as.character(cl), rep("a", 10))
})

test_that("rows missing different variables are grouped without mixing them up", {
  ## the fallback groups rows by which variables they are missing and sends
  ## each group to prob() in one call; a row must land in the right group and
  ## come back in its original position. The oracle is prob(), not predict().
  set.seed(71)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 500, TRUE)),
    B = factor(sample(c("x", "y"), 500, TRUE)),
    C = factor(sample(c("p", "q"), 500, TRUE)),
    D = factor(sample(c("u", "v"), 500, TRUE))
  )
  m <- full(d, lambda = 1)
  vs <- sevt_varnames(m)
  q <- d[1:10, vs]
  q[c(1L, 4L), "B"] <- NA                 # one pattern
  q[c(2L, 7L), "C"] <- NA                 # another
  q[9L, c("B", "C")] <- NA                # a third
  got <- predict(m, newdata = q, class = "A", prob = TRUE, log = TRUE)

  want <- t(vapply(seq_len(nrow(q)), function(i) {
    x <- unlist(lapply(q[i, vs], as.character))
    names(x) <- vs
    res <- vapply(m$tree$A, function(cv) {
      x[["A"]] <- cv
      prob(m, as.data.frame(t(x[!is.na(x)]), stringsAsFactors = FALSE),
           log = TRUE)
    }, 1.0)
    res[is.nan(res)] <- -Inf
    res - log(sum(exp(res)))
  }, numeric(length(m$tree$A))))

  expect_equal(unname(got), unname(want))
  expect_false(any(is.na(got)))
  ## rows 3, 5, 6, 8, 10 are complete and took the compiled path; they must
  ## agree with the same oracle
  expect_equal(unname(got[c(3, 5, 6, 8, 10), ]), unname(want[c(3, 5, 6, 8, 10), ]))
})
