context("basic model creation of stratified event trees")

test_that("test that the stratified event tree is created with the right order I", {

  ev <- strt_ev_tree.list(x = list(A=c(1,2), B =c("x","y"), C=c("3","4")))
  expect_true(all(names(ev$tree) == c("A","B","C")))

})

test_that("test that the stratified event tree is created with the right order II", {

  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- strt_ev_tree.data.frame(x = DD,order = c("B","A"))
  expect_true(all(names(ev$tree) == c("B","A")))

})



