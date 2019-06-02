context("testing util functions")

test_that("find_stage find the correct stage", {
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- strt_ev_tree(x = DD,order = c("B","A"))
  sevt <- staged_ev_tree(ev)
  stg1 <- stagedtrees:::find_stage(sevt, path = c("a"))
  stg2 <- stagedtrees:::find_stage(sevt, path = c("b"))
  expect_equal(c(stg1,stg2), c("1","2"))
})
  

#test_that("find_paths find the correct paths" ,{
#  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
#  #ev <- strt_ev_tree(x = DD,order = c("B","A"))
#  #sevt <- staged_ev_tree(ev)
#  #pth <- stagedtrees:::find_paths(paths = sevt$paths$A, stage = 1)
#  #expect_equal(as.character(pth[1,1]), "a")
#})


context("testing distance functions")

ds<- c(lp, ry, kl, tv, hl, bh, cd)

test_that("d(x,x) is 0", {
  x <- runif(10)
  x <- x / sum(x)
  expect_true(all(lapply(ds, function(d) d(x,x)) == 0))
})


test_that("d(x,y) is >= 0", {
  x <- runif(10)
  x <- x / sum(x)
  
  y <- runif(10)
  y <- y / sum(y)
  
  alpha <- runif(1, min = 1.1, max = 10)
  p <- rbinom(1, 20, 0.2)
  expect_true(all(lapply(ds, function(d) d(x,y, alpha = alpha, p = p)) >= 0))
})

test_that("d(x,y) == d(y,x)", {
  x <- runif(10, min = 1, max = 2)
  x <- x / sum(x)
  
  y <- runif(10, min = 1, max = 2)
  y <- y / sum(y)
  
  alpha <- runif(1, min = 1.1, max = 10)
  p <- rbinom(1, 20, 0.2)
  a <- sapply(ds, function(d) d(x, y, alpha = alpha, p = p))
  b <- sapply(ds, function(d) d(y, x, alpha = alpha, p = p))
  expect_true(all( abs(a - b) < 1e-12))
})



