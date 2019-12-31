context("testing util functions")

test_that("noisy_xor works as expected", {
  expect_true(stagedtrees:::noisy_xor(c(-1,-1), 0) == 1)
  expect_true(stagedtrees:::noisy_xor(c(-1,-1,-1), 0) == -1)
  expect_true(stagedtrees:::noisy_xor(c(-1, 1), 0) == -1)
  expect_true(stagedtrees:::noisy_xor(c(1, -1), 0) == -1)
})

test_that("new_lable produce a new label", {
  labels <- c(1:5, "6", "A", "B", "C")
  expect_false(stagedtrees:::new_label(labels) %in% labels)
})

test_that("uni_idx works as expected", {
  expect_silent(ui <-
                  stagedtrees:::uni_idx(list(
                    A = c(1, 2, 3), B = c(1, 2, 3)
                  )))
  expect_true(all(ui$A == c("A:1", "A:2", "A:3")))
})

test_that("find_stage find the correct stage", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), B = as.factor(c("a", "b", "a", "b")))
  ev <- strt_ev_tree(x = DD, order = c("B", "A"))
  sevt <- staged_ev_tree(ev)
  stg1 <- stagedtrees:::find_stage(sevt, path = c("a"))
  stg2 <- stagedtrees:::find_stage(sevt, path = c("b"))
  expect_equal(c(stg1, stg2), c("1", "2"))
})


context("testing distance functions")

ds <- c(lp, ry, kl, tv, hl, bh, cd)

test_that("lp(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
    
    p <- rbinom(1, 20, 0.2)
    expect_equal(lp(!!x,!!x, p = !!p), 0)
  })
})

test_that("ry(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
    
    alpha <- runif(1, min = 1.1, max = 10)
    expect_equal(ry(!!x,!!x, alpha = !alpha), 0)
  })
})

test_that("kl(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
    
    expect_equal(kl(!!x,!!x), 0)
  })
})

test_that("tv(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
    
    expect_equal(tv(!!x,!!x), 0)
  })
})

test_that("hl(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
    
    expect_equal(hl(!!x,!!x), 0)
  })
})

test_that("bh(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
    
    expect_equal(bh(!!x, !!x), 0)
  })
})

################# positive


test_that("lp(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    p <- rbinom(1, 20, 0.2)
    expect_gte(lp(!!x,!!y, p = !!p), 0)
  })
})

test_that("ry(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    alpha <- runif(1, min = 1.1, max = 10)
    expect_gte(ry(!!x,!!y, alpha = !alpha), 0)
  })
})

test_that("kl(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_gte(kl(!!x,!!y), 0)
  })
})

test_that("tv(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_gte(tv(!!x,!!y), 0)
  })
})

test_that("hl(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_gte(hl(!!x,!!y), 0)
  })
})

test_that("bh(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_gte(bh(!!x, !!y), 0)
  })
})

################### symmetric

test_that("lp(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    p <- rbinom(1, 20, 0.2)
    expect_equal(lp(!!x,!!y, p = !!p), 
                 lp(!!y, !!x, p = !!p))
  })
})

test_that("ry(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    alpha <- runif(1, min = 1.1, max = 10)
    expect_equal(ry(!!x,!!y, alpha = !alpha), 
                 ry(!!y,!!x, alpha = !alpha))
  })
})

test_that("kl(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_equal(kl(!!x,!!y), kl(!!y,!!x))
  })
})

test_that("tv(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_equal(tv(!!x,!!y), tv(!!y,!!x))
  })
})

test_that("hl(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_equal(hl(!!x,!!y), hl(!!y,!!x))
  })
})

test_that("bh(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_equal(bh(!!x, !!y), bh(!!y, !!x))
  })
})
