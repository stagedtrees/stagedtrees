context("info functions")

test_that("test that nvar_sevt return number of var", {
  ev <-
    staged_ev_tree.list(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_equal(nvar_sevt(ev), 3)

  ev <- staged_ev_tree.list(x = list(
    A = c(1, 2)
  ))
  expect_equal(nvar_sevt(ev), 1)
})


test_that("get_stage", {
  ev <-
    staged_ev_tree(list(
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
    staged_ev_tree(list(
      A = c("1", "2"),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_equivalent(get_stage(ev, get_path(ev, "B", "1")[1, ]), "1")
  expect_equivalent(get_stage(ev, get_path(ev, "C", "1")[1, ]), "1")
  expect_equivalent(get_stage(ev, get_path(ev, "C", "1")[2, ]), "1")
})
