test_that("resolve_score returns NULL for a function", {
  expect_null(resolve_score(function(x) -BIC(x)))
})

test_that("resolve_score returns the registry entry for a known name", {
  s <- resolve_score("BIC")
  expect_type(s, "list")
  expect_true(all(c("full", "delta") %in% names(s)))
  expect_true(is.function(s$full))
  expect_true(is.function(s$delta))
})

test_that("resolve_score aborts on an unknown name or a bad type", {
  expect_error(resolve_score("NOTASCORE"), regexp = "predefined scores")
  expect_error(resolve_score(42), regexp = "must be a function or a string")
  expect_error(resolve_score(c("BIC", "AIC")), regexp = "must be a function")
})

test_that("registry full and delta views agree for every score", {
  # for each predefined score, the delta view must predict the change that the
  # full view reports after an actual join
  set.seed(11)
  m <- full(generate_xor_dataset(p = 4, n = 200), lambda = 1)
  nobs <- attr(m$ll, "nobs")
  for (nm in names(stagedtrees:::.stages_scores)) {
    sc <- stagedtrees:::.stages_scores[[nm]]
    for (v in sevt_varnames(m)[-1]) {
      stg <- unique(m$stages[[v]])
      if (length(stg) < 2) next
      k <- length(m$tree[[v]])
      joined <- join_stages_unsafe(m, v, stg[1], stg[2])
      observed <- sc$full(joined) - sc$full(m)
      predicted <- sc$delta(
        join_ll_delta(m$prob[[v]][[stg[1]]], m$prob[[v]][[stg[2]]],
                      m$lambda, k),
        -(k - 1), nobs
      )
      expect_equal(observed, predicted,
                   info = paste("score", nm, "variable", v))
    }
  }
})

test_that("join_ll_delta matches the ll update of join_stages_unsafe", {
  set.seed(12)
  m <- full(generate_xor_dataset(p = 4, n = 200), lambda = 1)
  n_checked <- 0
  for (v in sevt_varnames(m)[-1]) {
    stg <- unique(m$stages[[v]])
    k <- length(m$tree[[v]])
    for (i in seq_along(stg)) {
      for (j in seq_len(i - 1)) {
        joined <- join_stages_unsafe(m, v, stg[i], stg[j])
        d <- join_ll_delta(m$prob[[v]][[stg[i]]], m$prob[[v]][[stg[j]]],
                           m$lambda, k)
        expect_equal(as.numeric(joined$ll), as.numeric(m$ll) + d)
        expect_equal(attr(joined$ll, "df"), attr(m$ll, "df") - (k - 1))
        n_checked <- n_checked + 1
      }
    }
  }
  expect_gt(n_checked, 0)
})

test_that("stages_bhc: string and function scores give identical results", {
  # the fast path compares a directly computed delta while the slow path
  # compares two full scores; ties are resolved with >=, so this guards
  # against a rounding difference selecting a different pair
  for (seed in 1:25) {
    set.seed(seed)
    DD <- generate_xor_dataset(p = 4, n = 100)
    m <- full(DD, lambda = 1)
    fast <- stages_bhc(m, score = "BIC")
    slow <- stages_bhc(m, score = function(x) -BIC(x))
    expect_equal(
      lapply(stages(fast), as.character),
      lapply(stages(slow), as.character),
      info = paste("seed", seed)
    )
    expect_equal(as.numeric(logLik(fast)), as.numeric(logLik(slow)),
                 info = paste("seed", seed))
    expect_equal(attr(logLik(fast), "df"), attr(logLik(slow), "df"),
                 info = paste("seed", seed))
    expect_equal(fast$score$value, slow$score$value, info = paste("seed", seed))
  }
})

test_that("stages_bhc: AIC string and function agree", {
  set.seed(7)
  m <- full(generate_xor_dataset(p = 4, n = 150), lambda = 1)
  fast <- stages_bhc(m, score = "AIC")
  slow <- stages_bhc(m, score = function(x) -AIC(x))
  expect_equal(lapply(stages(fast), as.character),
               lapply(stages(slow), as.character))
  expect_equal(fast$score$value, slow$score$value)
})

test_that("stages_bhc: max_iter and scope behave the same on both paths", {
  set.seed(8)
  m <- full(generate_xor_dataset(p = 4, n = 150), lambda = 1)
  v <- sevt_varnames(m)[2]
  for (mi in c(0, 1, 2)) {
    expect_equal(
      lapply(stages(stages_bhc(m, score = "BIC", max_iter = mi)), as.character),
      lapply(stages(stages_bhc(m, score = function(x) -BIC(x),
                               max_iter = mi)), as.character),
      info = paste("max_iter", mi)
    )
  }
  expect_equal(
    lapply(stages(stages_bhc(m, score = "BIC", scope = v)), as.character),
    lapply(stages(stages_bhc(m, score = function(x) -BIC(x),
                             scope = v)), as.character)
  )
})

test_that("stages_bhc: ignored stages are untouched on the fast path", {
  set.seed(9)
  m <- full(generate_xor_dataset(p = 4, n = 150),
            lambda = 1, join_unobserved = TRUE)
  fast <- stages_bhc(m, score = "BIC")
  slow <- stages_bhc(m, score = function(x) -BIC(x))
  expect_equal(lapply(stages(fast), as.character),
               lapply(stages(slow), as.character))
})

test_that("stages_bhc rejects an unknown score name", {
  m <- full(generate_xor_dataset(p = 3, n = 50), lambda = 1)
  expect_error(stages_bhc(m, score = "NOPE"), regexp = "predefined scores")
})

test_that("stages_bhc stores a function in $score$f for both paths", {
  m <- full(generate_xor_dataset(p = 3, n = 50), lambda = 1)
  expect_true(is.function(stages_bhc(m, score = "BIC")$score$f))
  expect_true(is.function(stages_bhc(m, score = function(x) -BIC(x))$score$f))
})
