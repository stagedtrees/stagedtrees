test_that("stages_hclust works when a variable has exactly 1 non-ignored stage (B-B3)", {
  m <- indep(data.frame(
    X = factor(c("a", "b")),
    Y = factor(c("1", "2"))
  ), lambda = 1)
  # Y has exactly 1 unique stage in the independence model
  for (dist in c("totvar", "hellinger", "euclidean", "manhattan")) {
    expect_no_error(stages_hclust(m, distance = dist, k = 1))
  }
  expect_s3_class(stages_hclust(m, k = 1), "sevt")
})

## relabel_by_group replaces a loop that scanned the whole situation vector
## once per cluster. The part worth pinning is what it must NOT touch: stages
## excluded from the clustering via `ignore` are absent from `groups`, and
## those situations have to keep their original labels rather than become NA.

test_that("relabel_by_group maps each situation through its stage's cluster", {
  old <- c("a", "b", "a", "c", "b")
  groups <- c(a = 1L, b = 2L, c = 1L)
  expect_identical(stagedtrees:::relabel_by_group(old, old, groups),
                   c("1", "2", "1", "1", "2"))
})

test_that("relabel_by_group leaves stages absent from the clustering alone", {
  old <- c("a", "UNOBSERVED", "b", "UNOBSERVED")
  groups <- c(a = 1L, b = 2L)          # UNOBSERVED was ignored, so not clustered
  got <- stagedtrees:::relabel_by_group(old, old, groups)
  expect_identical(got, c("1", "UNOBSERVED", "2", "UNOBSERVED"))
  expect_false(anyNA(got))
})

test_that("stages_hclust with ignore preserves the ignored stages", {
  set.seed(11)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 300, TRUE)),
    B = factor(sample(c("x", "y", "z"), 300, TRUE)),
    C = factor(sample(c("p", "q"), 300, TRUE))
  )
  m <- full(d, lambda = 1)
  v <- sevt_varnames(m)[3]
  keep <- stages(m)[[v]][1]
  r <- stages_hclust(m, ignore = keep)
  ## every situation that was in the ignored stage still is
  was <- stages(m)[[v]] == keep
  expect_true(any(was))
  expect_identical(as.character(stages(r)[[v]][was]),
                   rep(keep, sum(was)))
  expect_false(anyNA(stages(r)[[v]]))
})
