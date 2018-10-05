context("testing util functions")

test_that("find_stage find the correct stage", {
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- strt_ev_tree(x = DD,order = c("B","A"))
  sevt <- staged_ev_tree(ev)
  stg1 <- stagedtrees:::find_stage(paths = sevt$paths$A, path = c("a"))
  stg2 <- stagedtrees:::find_stage(paths = sevt$paths$A, path = c("b"))
  expect_equal(c(stg1,stg2), c(1,2))
})


test_that("find_paths find the correct paths" ,{
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- strt_ev_tree(x = DD,order = c("B","A"))
  sevt <- staged_ev_tree(ev)
  pth <- stagedtrees:::find_paths(paths = sevt$paths$A, stage = 1)
  expect_equal(as.character(pth[1,1]), "a")
})


