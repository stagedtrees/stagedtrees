## join_stages

test_that("join_stages works as expected", {
  mod <- sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), full = TRUE)
  expect_silent(mod1 <- join_stages(mod, "B", "1", "2"))
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("1", "1"))
})

test_that("join_stages works as expected on tree with probs", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), q = 0)
  expect_silent(mod1 <- join_stages(mod, "B", "1", "2"))
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("1", "1"))
})

test_that("join_stages works as expected on fitted tree", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ), q = 0)
  mod <- sevt_fit(mod, sample_from(mod, 100), lambda = 0)
  expect_silent(mod1 <- join_stages(mod, "B", "1", "2"))
  expect_identical(mod1$stages$C, mod$stages$C)
  expect_identical(mod1$stages$B, c("1", "1"))
})


test_that("join_all joins all stages", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb"),
    C = c("c", "cc")
  ), q = 0.5)
  mod1 <- join_all(mod, "C", unique(mod$stages$C))
  expect_length(unique(mod1$stages$C), 1)
})


test_that("join_all works for 2 stages", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb"),
    C = c("c", "cc")
  ), q = 0)
  mod1 <- join_all(mod, "C", c("1", "3"))
  mod2 <- join_stages(mod, "C", "1", "3")
  expect_identical(mod1, mod2)
})

test_that("join_all ignores stages in ignore argument", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb"),
    C = c("c", "cc")
  ), q = 0)
  mod1 <- join_all(mod, "C", c("1", "3", "4"), ignore = "4")
  mod2 <- join_stages(mod, "C", "1", "3")
  expect_identical(mod1, mod2)
})

test_that("join_stages_unsafe preserves NA probabilities as zero counts", {
  # the NA -> 0 step feeding ct1/ct2 is done by indexed assignment rather than
  # ifelse; unlike ifelse it carries p1's attributes through, which must stay
  # unobservable because attr(, "n") is overwritten before the vector is read
  set.seed(31)
  DD <- generate_xor_dataset(p = 4, n = 120)
  DD[sample(nrow(DD), 15), 2] <- NA
  for (lam in c(0, 1)) {
    m <- full(DD, lambda = lam)
    for (v in sevt_varnames(m)[-1]) {
      stg <- unique(m$stages[[v]])
      if (length(stg) < 2) next
      j <- join_stages_unsafe(m, v, stg[1], stg[2])
      p <- j$prob[[v]][[stg[1]]]
      expect_equal(sum(p), 1)
      expect_equal(attr(p, "n"),
                   sum(attr(m$prob[[v]][[stg[1]]], "n"),
                       attr(m$prob[[v]][[stg[2]]], "n")))
      expect_named(p, names(m$prob[[v]][[stg[1]]]))
      expect_false(is.null(names(p)))
    }
  }
})

test_that("join_stages_unsafe ll and df updates stay consistent with logLik", {
  set.seed(32)
  m <- full(generate_xor_dataset(p = 4, n = 200), lambda = 1)
  for (v in sevt_varnames(m)[-1]) {
    stg <- unique(m$stages[[v]])
    if (length(stg) < 2) next
    j <- join_stages_unsafe(m, v, stg[1], stg[2])
    # incremental update must match a from-scratch recomputation
    refit <- sevt_fit(j, lambda = m$lambda)
    expect_equal(as.numeric(j$ll), as.numeric(logLik(refit)))
    expect_equal(attr(j$ll, "df"), attr(logLik(refit), "df"))
  }
})
