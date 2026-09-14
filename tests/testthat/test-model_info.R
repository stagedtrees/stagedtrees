test_that("test that sevt_nvar return number of var", {
  ev <-
    sevt.list(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_equal(sevt_nvar(ev), 3)

  ev <- sevt.list(x = list(
    A = c(1, 2)
  ))
  expect_equal(sevt_nvar(ev), 1)
})


test_that("get_stage", {
  ev <-
    sevt(list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_equal(get_stage(ev, c("1", "x")), expected = "1")
  expect_equal(get_stage(ev, c("1", "y")), expected = "1")
  expect_equal(get_stage(ev, c("2")), expected = "1")
})


test_that("get_path", {
  ev <-
    sevt(list(
      A = c("1", "2"),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_equal(get_stage(ev, get_path(ev, "B", "1")[1, ]), "1",
               ignore_attr = TRUE)
  expect_equal(get_stage(ev, get_path(ev, "C", "1")[1, ]), "1",
               ignore_attr = TRUE)
  expect_equal(get_stage(ev, get_path(ev, "C", "1")[2, ]), "1",
               ignore_attr = TRUE)
})


test_that("sevt_df", {
  sev <- sevt(list(c("a", "b"), c("c", "d")))
  expect_equal(sevt_df(sev), 2)
})

test_that("sevt_df C1: robust to root entry and scrambled stages order", {
  data(Titanic)
  m <- full(Titanic, lambda = 1)
  expected <- sevt_df(m)                # 30 for clean full(Titanic)

  # With root entry fabricated in $stages
  m_root <- m
  m_root$stages[["Class"]] <- "1"      # root entry inserted
  expect_equal(sevt_df(m_root), expected)

  # With $stages in scrambled order
  m_scram <- m
  m_scram$stages <- m_scram$stages[sample(length(m_scram$stages))]
  expect_equal(sevt_df(m_scram), expected)
})

test_that("logLik df attribute uses sevt_df and is robust to root entry (C1)", {
  data(Titanic)
  m <- full(Titanic, lambda = 1)
  expect_equal(attr(logLik(m), "df"), sevt_df(m))

  m2 <- m
  m2$stages[["Class"]] <- "1"
  m2$ll <- NULL            # force recompute
  expect_equal(attr(logLik(m2), "df"), sevt_df(m))
})
