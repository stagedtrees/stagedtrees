context("info functions")

test_that("test that nvar.sevt return number of var", {
  ev <-
    staged_ev_tree.list(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_equal(nvar.sevt(ev), 3)
  
  ev <- staged_ev_tree.list(x = list(
    A = c(1, 2)
  )) 
  expect_equal(nvar.sevt(ev), 1)
  
})
