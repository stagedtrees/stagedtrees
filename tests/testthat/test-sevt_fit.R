test_that("test that the staged event tree is fitted", {
  DD <-
    data.frame(
      A = as.factor(c(1, 2, 2, 1)),
      B = as.factor(c("a", "b", "a", "b")),
      C = as.factor(c("SD", "de", "rew", "tyu")),
      D = as.factor(c("a", "uu", "a", "uu"))
    )
  ev <- sevt_fit(sevt(x = DD), data = DD, lambda = 0)
  for (var in c("A", "B", "C", "D")) {
    expect_equal(as.numeric(ev$prob[[var]][[1]]), as.numeric(table(DD[var]) / sum(table(DD[var]))))
  }
})


test_that("test that probabilities are probabilities (indep model)", {
  DD <- as.data.frame(sapply(1:4, function(i) {
    return(as.factor(sample(
      c(0, 1, 2),
      size = 100,
      replace = TRUE
    )))
  }))
  sev <- sevt_fit(sevt(DD), data = DD, lambda = 1)
  for (i in 1:4) {
    for (j in 1:3) {
      expect_gte(sev$prob[[i]][[1]][j], 0) ## test prob are >=0
    }
    expect_equal(sum(sev$prob[[i]][[1]]), 1) ## test prob sum up to one
  }

  sev <- indep(DD, lambda = 1)
  for (i in 1:4) {
    for (j in 1:3) {
      expect_gte(sev$prob[[i]][[1]][j], 0) ## test prob are >=0
    }
    expect_equal(sum(sev$prob[[i]][[1]]), 1) ## test prob sum up to one
  }
})

test_that("test that probabilities are probabilities (full model)", {
  DD <- as.data.frame(sapply(1:4, function(i) {
    return(as.factor(sample(
      c(0, 1, 2),
      size = 100,
      replace = TRUE
    )))
  }))
  sev <- sevt_fit(sevt(DD, full = TRUE),
    data = DD,
    lambda = 1
  )
  for (i in 1:4) {
    for (s in 1:length(sev$prob[[i]])) {
      for (j in 1:3) {
        expect_gte(sev$prob[[i]][[s]][j], 0) ## test prob are >=0
      }
      expect_equal(sum(sev$prob[[i]][[s]]), 1) ## test prob sum up to one
    }
  }

  sev <- full(DD, lambda = 1)
  for (i in 1:4) {
    for (j in 1:3) {
      expect_gte(sev$prob[[i]][[1]][j], 0) ## test prob are >=0
    }
    expect_equal(sum(sev$prob[[i]][[1]]), 1) ## test prob sum up to one
  }
})


test_that("partial fitting", {
  mod1 <- full(PhDArticles)
  mod2 <- mod1
  mod2$stages$Married[c(1, 2, 3, 7, 9)] <- "aa"
  mod2a <- sevt_fit(mod2, scope = "Married")
  mod2b <- sevt_fit(mod2)
  expect_identical(logLik(mod2a), logLik(mod2b))
})


test_that("partial fitting should not change lambda", {
  mod1 <- full(PhDArticles, lambda = 0)
  mod2 <- mod1
  mod2$stages$Married[c(1, 2, 3, 7, 9)] <- "aa"
  expect_warning(mod2a <- sevt_fit(mod2, scope = "Married", lambda = 3))
  mod2b <- sevt_fit(mod2)
  expect_identical(logLik(mod2a), logLik(mod2b))
  expect_identical(mod2a$lambda, mod2b$lambda)
})


test_that("partial fitting throws warning if data or lambda is provided", {
  mod1 <- full(PhDArticles, lambda = 0)
  expect_warning(sevt_fit(mod1, scope = "Married", lambda = 3))
  expect_warning(sevt_fit(mod1, scope = "Married", data = PhDArticles))
})

test_that("sevt_fit fails if no data is provided",{
  mod <- random_sevt(list("A" = c("a", "aa"),
                          "B" = c("b", "bb", "bbb")))
  expect_error(sevt_fit(mod))
})

test_that("sevt_fit warns if no lambda is provided",{
  mod <- random_sevt(list("A" = c("a", "aa"),
                          "B" = c("b", "bb", "bbb")))
  expect_warning(mod <- sevt_fit(mod, sample_from(mod, 100)))
})
