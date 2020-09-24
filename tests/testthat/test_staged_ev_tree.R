context("basic model creation of staged event trees")

test_that("test that the creator works for list", {
  ev <-
    staged_ev_tree(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_is(ev, "sevt")
  ev <-
    staged_ev_tree(x = list(
      c(1, 2),
      c("x", "y"),
      c("3", "4")
    ))
  expect_is(ev, "sevt")
  expect_error(staged_ev_tree(x = list(
    NULL,
    c("x", "y"),
    c("3", "4")
  )))
})

test_that("test that the creator works for data.frame", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
  ev <- staged_ev_tree(x = DD, order = c("B", "A"))
  expect_is(ev, "sevt")
})

test_that("test that the creator works for tables", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
  DD <- table(DD)
  ev <- staged_ev_tree(x = DD, order = c("B", "A"))
  expect_is(ev, "sevt")
})

test_that("test that the staged event tree is created with the right order I", {
  ev <-
    staged_ev_tree(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_true(all(names(ev$tree) == c("A", "B", "C")))
})

test_that("test that the staged event tree is created with the right order II", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
  ev <- staged_ev_tree(x = DD, order = c("B", "A"))
  expect_true(all(names(ev$tree) == c("B", "A")))
})

context("fitting of staged event tree")

test_that("test that the staged event tree is fitted", {
  DD <-
    data.frame(
      A = as.factor(c(1, 2, 2, 1)),
      B = as.factor(c("a", "b", "a", "b")),
      C = as.factor(c("SD", "de", "rew", "tyu")),
      D = as.factor(c("a", "uu", "a", "uu"))
    )
  ev <- sevt.fit(staged_ev_tree(x = DD), data = DD, lambda = 0)
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
  sev <- sevt.fit(staged_ev_tree(DD), data = DD, lambda = 1 )
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
  sev <- sevt.fit(staged_ev_tree(DD, full = TRUE), data = DD, 
                  lambda = 1)
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


test_that("test indep model", {
  DD <- as.data.frame(sapply(1:4, function(i) {
    return(as.factor(sample(
      c(0, 1, 2),
      size = 100,
      replace = TRUE
    )))
  }))
  sev1 <- sevt.fit(staged_ev_tree(DD), data = DD, lambda = 1)

  sev2 <- indep(DD, lambda = 1)

  expect_true(compare_stages(sev1, sev2))
})
