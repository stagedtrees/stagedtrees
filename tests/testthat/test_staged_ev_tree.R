context("basic model creation of staged event trees")

test_that("test that the creator works for list",{
  ev <- staged_ev_tree.list(x = list(A=c(1,2), B =c("x","y"), C=c("3","4")))
  expect_is(ev,"staged_ev_tree")
})

test_that("test that the creator works for data.frame",{
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- staged_ev_tree(x = DD,order = c("B","A"))
  expect_is(ev,"staged_ev_tree")
})

test_that("test that the staged event tree is created with the right order I", {

  ev <- staged_ev_tree(x = list(A=c(1,2), B =c("x","y"), C=c("3","4")))
  expect_true(all(names(ev$tree) == c("A","B","C")))

})

test_that("test that the staged event tree is created with the right order II", {

  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- staged_ev_tree(x = DD,order = c("B","A"))
  expect_true(all(names(ev$tree) == c("B","A")))

})

context("fitting of staged event tree")

test_that("test that the staged event tree is fitted", {

  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- staged_ev_tree( x = DD, fit = TRUE, lambda = 0)
  expect_equal(ev$prob$A[[1]], table(DD[ "A" ]) / sum(table(DD[ "A" ])))
})
