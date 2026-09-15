test_that("find_stage find the correct stage", {
  DD <-
    data.frame(
      A = as.factor(c(1, 2, 2, 1)),
      B = as.factor(c("a", "b", "a", "b"))
    )
  sevt <- sevt(DD, order = c("B", "A"), full = TRUE)
  stg1 <- find_stage(sevt, path = c("a"))
  stg2 <- find_stage(sevt, path = c("b"))
  expect_equal(c(stg1, stg2), c("1", "2"))
})

test_that("find_stage find the correct stage (scrambled stages)", {
  DD <-
    data.frame(
      A = as.factor(c(1, 2, 2, 1)),
      B = as.factor(c("a", "b", "a", "b")),
      C = as.factor(c("1", "2", "3", "3"))
    )
  sevt <- sevt(DD, order = c("B", "A", "C"), full = TRUE)
  sevt$stages <- sevt$stages[2:1]
  stg1 <- find_stage(sevt, path = c("a"))
  stg2 <- find_stage(sevt, path = c("b"))
  stg3 <- find_stage(sevt, path = c("a", "1"))
  stg4 <- find_stage(sevt, path = c("b", "1"))
  expect_equal(c(stg1, stg2, stg3, stg4), c("1", "2", "1", "3"))
})

test_that("find_stage find the correct stage (scrambled stages 2)", {
  DD <- generate_linear_dataset(p = 6, n = 10)
  sevt <- sevt(DD, full = TRUE)
  replicate(5, {
    pth <- sample(c("1", "-1"), size = 4, replace = TRUE)
    stg <- find_stage(sevt, pth)
    sevt$stages <- sevt$stages[sample(seq_along(sevt$stages))]
    expect_equal(stg, find_stage(sevt, pth))
  })
})

test_that("tree_idx indexes every path of a tree correctly", {
  # exhaustive over all paths, both complete modes, incl. a 1-level variable
  trees <- list(
    list(A = c("a", "b"), B = c("x", "y")),
    list(A = c("a", "b", "c"), B = c("x", "y"), C = c("p", "q", "r", "s")),
    list(S = "only", T = c("u", "v"))
  )
  for (tr in trees) {
    for (k in seq_along(tr)) {
      grid <- expand.grid(tr[seq_len(k)], stringsAsFactors = FALSE)
      # the non-complete index enumerates the level-k nodes as 1..prod(sizes)
      seen <- vapply(seq_len(nrow(grid)), function(r) {
        tree_idx(as.character(unlist(grid[r, , drop = FALSE])), tr)
      }, FUN.VALUE = 1)
      expect_setequal(seen, seq_len(prod(lengths(tr[seq_len(k)]))))
    }
  }
})

test_that("tree_idx errors on a value that is not a level", {
  tr <- list(A = c("a", "b"), B = c("x", "y"))
  expect_error(tree_idx(c("ZZZ", "x"), tr), regexp = "not a level")
  expect_error(tree_idx(c("a", "ZZZ"), tr), regexp = "not a level")
  expect_error(tree_idx("ZZZ", tr), regexp = "not a level")
})

test_that("tree_idx complete mode offsets by one level size", {
  tr <- list(A = c("a", "b"), B = c("x", "y"))
  # complete indexing counts the parent node itself, so it is strictly larger
  for (p in list(c("a", "x"), c("b", "y"))) {
    expect_gt(tree_idx(p, tr, complete = TRUE), tree_idx(p, tr))
  }
})

test_that("tree_idx returns NA for an empty path (A-B4)", {
  # stages(m)[[character(0)]] reaches tree_idx with a zero-length path;
  # it must not error and must not index path[[0]]
  tr <- list(A = c("a", "b"), B = c("x", "y"))
  expect_identical(tree_idx(character(0), tr), NA_real_)
  expect_no_error(tree_idx(character(0), tr))
})

test_that("find_stage var argument matches the derived name", {
  # var is a performance shortcut for callers that already know the variable;
  # it must agree with what find_stage would derive for itself
  set.seed(51)
  d <- as.data.frame(lapply(1:5, function(i)
    factor(sample(letters[1:3], 300, replace = TRUE))),
    col.names = paste0("V", 1:5))
  for (ju in c(TRUE, FALSE)) {
    m <- full(d, lambda = 1, join_unobserved = ju)
    vars <- sevt_varnames(m)
    for (k in seq_len(length(vars) - 1L)) {
      paths <- expand.grid(m$tree[seq_len(k)], stringsAsFactors = FALSE)
      for (r in seq_len(nrow(paths))) {
        p <- as.character(unlist(paths[r, , drop = FALSE]))
        expect_identical(
          find_stage(m, p, var = vars[k + 1L]),
          find_stage(m, p)
        )
      }
    }
  }
})
