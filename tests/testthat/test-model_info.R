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
