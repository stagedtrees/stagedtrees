## make_ctables computes each prefix's counts by summing the next prefix over
## its last variable. That cascade sums trailing dimensions BY POSITION, where
## the code it replaced selected margins by name, so the cases that matter are
## the ones where position and name disagree. Both were found by other test
## files rather than by a 250-model equivalence grid, which is why they are
## pinned directly here.

ct_counts <- function(m, data, ...) {
  lapply(stagedtrees:::make_ctables(m, data, ...), function(x) as.vector(unclass(x)))
}

test_that("counts do not depend on the table's own dimension order", {
  data(Titanic)
  vars <- names(dimnames(Titanic))
  ## same tree, reached from a table whose dimensions are in a different order
  for (perm in list(4:1, c(2, 4, 1, 3), c(3, 1, 4, 2))) {
    ord <- vars[perm]
    m <- indep(Titanic, lambda = 1, order = ord)
    ct <- stagedtrees:::make_ctables(m, Titanic)
    expect_named(ct, ord)
    ## every prefix must total the number of observations
    for (i in seq_along(ord)) {
      expect_equal(sum(ct[[i]]), sum(Titanic))
      ## each prefix holds the counts of that prefix's variables. Compare the
      ## multiset rather than the layout: marginalising the wrong variables
      ## changes which counts exist, which is the failure being guarded
      ## against, while the ftable's row/column arrangement is not part of
      ## what this test is pinning.
      expect_equal(sort(as.vector(unclass(ct[[i]]))),
                   sort(as.vector(margin.table(Titanic, ord[1:i]))))
    }
    ## the first prefix is a plain named vector, so it can be compared exactly
    expect_equal(as.vector(ct[[1]]), as.vector(margin.table(Titanic, ord[1])))
    expect_equal(names(ct[[1]]), dimnames(Titanic)[[ord[1]]])
  }
})

test_that("a tree over a subset of the table's variables marginalises the rest", {
  data(Titanic)
  for (vars in list(c("Class", "Age"), c("Survived", "Sex", "Class"))) {
    m <- indep(Titanic, lambda = 1, order = vars)
    ct <- stagedtrees:::make_ctables(m, Titanic)
    expect_named(ct, vars)
    for (i in seq_along(vars)) expect_equal(sum(ct[[i]]), sum(Titanic))
    ## first variable's counts equal the table's own margin for it
    expect_equal(as.vector(ct[[1]]),
                 as.vector(margin.table(Titanic, vars[1])))
  }
})

test_that("the counts' storage mode is preserved, not silently promoted", {
  ## rowSums returns double whatever it is given. The type is not cosmetic: it
  ## reaches identical() through the "n" attribute of every fitted probability,
  ## which is how an earlier "obvious" optimisation in sevt_fit slipped a
  ## double in where an integer belonged.
  set.seed(6)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 120, TRUE)),
    B = factor(sample(c("x", "y", "z"), 120, TRUE))
  )
  m <- indep(d, lambda = 1)
  ## tabulating a data.frame gives integer counts
  expect_identical(typeof(table(d)), "integer")
  for (x in stagedtrees:::make_ctables(m, d)) {
    expect_identical(typeof(unclass(x)), "integer")
  }
  ## a table supplied directly keeps its own mode; Titanic happens to be double
  data(Titanic)
  expect_identical(typeof(Titanic), "double")
  mt <- indep(Titanic, lambda = 1)
  for (x in stagedtrees:::make_ctables(mt, Titanic)) {
    expect_identical(typeof(unclass(x)), "double")
  }
})

test_that("a data.frame and its table give the same counts", {
  set.seed(4)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 200, TRUE)),
    B = factor(sample(c("x", "y", "z"), 200, TRUE)),
    C = factor(sample(c("p", "q"), 200, TRUE))
  )
  m <- indep(d, lambda = 1)
  expect_equal(ct_counts(m, d), ct_counts(m, table(d)))
})

test_that("NA values are dropped from each prefix but not from the margin", {
  set.seed(5)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 200, TRUE)),
    B = factor(sample(c("x", "y"), 200, TRUE))
  )
  d$B[1:20] <- NA
  m <- indep(d, lambda = 1)
  ct <- stagedtrees:::make_ctables(m, d)
  ## A is complete, so its counts keep every observation ...
  expect_equal(sum(ct[[1]]), nrow(d))
  ## ... while the (A, B) table drops the rows whose B is missing
  expect_equal(sum(ct[[2]]), sum(!is.na(d$B)))
})
