mod <- random_sevt(list(c("1", "0"),
                        c("a", "aa", "aaa"),
                        c('bb', "1"),
                        c('-1', "1"),
                        c('la', "lala", "lalala")))

test_that("sample size is correct", {
  data <- sample_from(mod, 27)
  expect_equal(nrow(data), 27)
})

test_that("sample_from should return a data.frame with factors", {
  data <- sample_from(mod, 27)
  expect_s3_class(data, "data.frame")
  expect_s3_class(data$V1, "factor")
  expect_s3_class(data$V2, "factor")
  expect_s3_class(data$V3, "factor")
})


test_that("variables number and names are correct", {
  data <- sample_from(mod, 35)
  expect_equal(colnames(data), sevt_varnames(mod))
  expect_equal(colnames(data), names(mod$tree))
  
})

test_that("sampling with seed", {
  data <- sample_from(mod, 35, seed = 23)
  expect_equal(colnames(data), names(mod$tree))
  expect_equal(nrow(data), 35)
})



test_that("sampling of 1 observation works", {
  data <- sample_from(mod, 1)
  expect_equal(colnames(data), names(mod$tree))
  expect_equal(nrow(data), 1)
})


test_that("sampling of 0 or <0 should throw error", {
  expect_error(sample_from(mod, 0))
})

prob <- mod$prob
mod$prob <- NULL

test_that("sampling from a non fitted model shoudl throw error", {
  expect_error(sample_from(mod, 10))
})

# restore probabilities
mod$prob <- prob


test_that("sampling from a non sevt object shoudl throw error", {
  class(mod) <- "ajsjhhsajh"
  expect_error(sample_from(mod, 10))
})

test_that("sample_from works on 1-variable model (B-B1)", {
  m <- full(data.frame(X = factor(c("a", "b", "a"))), lambda = 1)
  expect_no_error(sample_from(m, size = 5))
  s <- sample_from(m, size = 5)
  expect_equal(ncol(s), 1L)
  expect_equal(nrow(s), 5L)
})

test_that("stndnaming works on 1-variable model (A-B3)", {
  m <- full(data.frame(X = factor(c("a", "b", "a"))), lambda = 1)
  expect_no_error(stndnaming(m))
  expect_s3_class(stndnaming(m), "sevt")
})

## Samples are now drawn one stage at a time rather than one observation at a
## time, so a given seed produces different values than before. What must hold
## is the distribution, the NA rules, and reproducibility within a version.

test_that("the sampled distribution matches the model", {
  set.seed(51)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 600, TRUE)),
    B = factor(sample(c("x", "y", "z"), 600, TRUE)),
    C = factor(sample(c("p", "q"), 600, TRUE))
  )
  m <- full(d, lambda = 1)
  N <- 40000
  set.seed(52)
  S <- sample_from(m, N)
  cells <- expand.grid(m$tree, stringsAsFactors = FALSE)
  want <- prob(m, cells)
  counts <- as.vector(table(factor(
    do.call(paste, c(S, sep = "|")),
    levels = do.call(paste, c(cells, sep = "|"))
  )))
  expect_equal(sum(want), 1, tolerance = 1e-8)
  expect_equal(sum(counts), N)
  ## One goodness-of-fit test over all cells, not a per-cell bound: with a
  ## dozen cells a 3-sigma band is crossed by chance a few percent of the
  ## time, which makes for a test that fails on an innocent seed.
  expect_gt(stats::chisq.test(counts, p = want)$p.value, 0.001)
})

test_that("a path reaching a stage that cannot be sampled gives NA, and it propagates", {
  ## a sparse model leaves some situations unobserved; with smoothing their
  ## branches are still reachable, so the sampler runs into them
  mk <- function(n, p, lv, seed) {
    set.seed(seed)
    as.data.frame(lapply(seq_len(p), function(i)
      factor(sample(letters[seq_len(lv)], n, replace = TRUE))),
      col.names = paste0("V", seq_len(p)))
  }
  m <- full(mk(40, 4, 3, 56), lambda = 1, join_unobserved = TRUE)
  vs <- sevt_varnames(m)
  set.seed(53)
  S <- sample_from(m, 4000)
  expect_gt(mean(!stats::complete.cases(S)), 0)   # the NA path is exercised

  ## once a variable is NA every later one is too
  M <- is.na(S)
  for (j in 2:ncol(M)) expect_false(any(M[, j - 1] & !M[, j]))

  ## and a value is NA exactly when its prefix reached an unsampleable stage
  checked <- 0
  for (i in 2:length(vs)) {
    ok <- !is.na(S[[vs[i - 1]]])
    if (!any(ok)) next
    pref <- S[ok, seq_len(i - 1), drop = FALSE]
    stg <- vapply(seq_len(nrow(pref)), function(r)
      stagedtrees:::find_stage(m, as.character(unlist(pref[r, ])), var = vs[i]), "")
    unsampleable <- stg %in% m$name_unobserved |
      vapply(stg, function(z) anyNA(m$prob[[vs[i]]][[z]]), TRUE)
    expect_identical(unname(unsampleable), is.na(S[[vs[i]]][ok]))
    checked <- checked + length(stg)
  }
  expect_gt(checked, 0)
})

test_that("sampling is reproducible and keeps the model's shape", {
  set.seed(54)
  d <- data.frame(
    A = factor(sample(c("a", "b"), 300, TRUE)),
    B = factor(sample(c("x", "y", "z"), 300, TRUE))
  )
  m <- full(d, lambda = 1)
  set.seed(55); s1 <- sample_from(m, 200)
  set.seed(55); s2 <- sample_from(m, 200)
  expect_identical(s1, s2)
  expect_identical(sample_from(m, 200, seed = 6), sample_from(m, 200, seed = 6))
  expect_s3_class(s1, "data.frame")
  expect_identical(names(s1), sevt_varnames(m))
  for (v in names(s1)) {
    expect_s3_class(s1[[v]], "factor")
    expect_identical(levels(s1[[v]]), m$tree[[v]])
  }
  expect_false(is.null(attr(s1, "seed")))
})
