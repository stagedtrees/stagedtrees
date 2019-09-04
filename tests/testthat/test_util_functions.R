context("testing util functions")

test_that("noisy_xor works as expected", {
   expect_true(stagedtrees:::noisy_xor(c(-1, -1), 0) == 1)
   expect_true(stagedtrees:::noisy_xor(c(-1, -1, -1), 0) == -1)
   expect_true(stagedtrees:::noisy_xor(c(-1, 1), 0) == -1)
   expect_true(stagedtrees:::noisy_xor(c(1,-1), 0) == -1)
})

test_that("new_lable produce a new label",{
  labels <- c(1:5, "6", "A", "B", "C")
  expect_false(stagedtrees:::new_label(labels) %in% labels)
})

test_that("uni_idx works as expected", {
  expect_silent(ui <- stagedtrees:::uni_idx(list(A = c(1,2,3), B = c(1,2,3))))
  expect_true(all(ui$A == c("A:1", "A:2", "A:3")))
})

test_that("find_stage find the correct stage", {
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- strt_ev_tree(x = DD,order = c("B","A"))
  sevt <- staged_ev_tree(ev)
  stg1 <- stagedtrees:::find_stage(sevt, path = c("a"))
  stg2 <- stagedtrees:::find_stage(sevt, path = c("b"))
  expect_equal(c(stg1,stg2), c("1","2"))
})
  

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



