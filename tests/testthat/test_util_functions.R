context("testing util functions")

test_that("noisy_xor works as expected", {
  expect_true(stagedtrees:::noisy_xor(c(-1, -1), 0) == 1)
  expect_true(stagedtrees:::noisy_xor(c(-1, -1, -1), 0) == -1)
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
    ), sep = ":"))
  expect_true(all(ui$A == c("A:1", "A:2", "A:3")))
})

test_that("find_stage find the correct stage", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), 
               B = as.factor(c("a", "b", "a", "b")))
  sevt <- sevt(DD, order = c("B", "A"), full = TRUE)
  stg1 <- stagedtrees:::find_stage(sevt, path = c("a"))
  stg2 <- stagedtrees:::find_stage(sevt, path = c("b"))
  expect_equal(c(stg1, stg2), c("1", "2"))
})

test_that("find_stage find the correct stage (scrambled stages)", {
  DD <-
    data.frame(A = as.factor(c(1, 2, 2, 1)), 
               B = as.factor(c("a", "b", "a", "b")),
               C = as.factor(c("1", "2", "3", "3")))
  sevt <- sevt(DD, order = c("B", "A", "C"), full = TRUE)
  sevt$stages <- sevt$stages[2:1]
  stg1 <- stagedtrees:::find_stage(sevt, path = c("a"))
  stg2 <- stagedtrees:::find_stage(sevt, path = c("b"))
  stg3 <- stagedtrees:::find_stage(sevt, path = c("a", "1")) 
  stg4 <- stagedtrees:::find_stage(sevt, path = c("b", "1")) 
  expect_equal(c(stg1, stg2, stg3, stg4), c("1", "2", "1", "3"))
})

test_that("find_stage find the correct stage (scrambled stages 2)", {
  DD <- generate_linear_dataset(n = 6, N = 10)
  sevt <- sevt(DD, full = TRUE)
  replicate(5, {
    pth <- sample(c("1", "-1"), size = 4, replace = TRUE)
    stg <- stagedtrees:::find_stage(sevt, pth)
    sevt$stages <- sevt$stages[sample(seq_along(sevt$stages))]
    expect_equal(stg, stagedtrees:::find_stage(sevt, pth))
  })
})

context("testing distance functions")

ds <- c(probdist.l1, probdist.l2, probdist.ry, 
        probdist.kl, probdist.tv, 
        probdist.hl, probdist.bh, probdist.cd)

test_that("l1(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)

    p <- rbinom(1, 5, 0.2) + 1
    expect_equal(probdist.l1(!!x, !!x), 0)
  })
})

test_that("l2(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
   
    expect_equal(probdist.l2(!!x, !!x), 0)
  })
})

test_that("ry(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)

    expect_equal(probdist.ry(!!x, !!x), 0)
  })
})

test_that("kl(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)

    expect_equal(probdist.kl(!!x, !!x), 0)
  })
})

test_that("tv(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)

    expect_equal(probdist.tv(!!x, !!x), 0)
  })
})

test_that("hl(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)

    expect_equal(probdist.hl(!!x, !!x), 0)
  })
})

test_that("bh(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)

    expect_equal(probdist.bh(!!x, !!x), 0)
  })
})

test_that("cd(x,x) is = 0", {
  replicate(10, {
    x <- runif(10, min = 1, max = 2)
    x <- x / sum(x)
    
    expect_equal(probdist.cd(!!x, !!x), 0)
  })
})

################# positive


test_that("l1(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)

    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)

    expect_gte(probdist.l1(!!x, !!y), 0)
  })
})


test_that("l2(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)
    
    expect_gte(probdist.l2(!!x, !!y), 0)
  })
})
test_that("ry(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)

    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)

    expect_gte(probdist.ry(!!x, !!y), 0)
  })
})

test_that("kl(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)

    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)

    expect_gte(probdist.kl(!!x, !!y), 0)
  })
})

test_that("tv(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)

    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)

    expect_gte(probdist.tv(!!x, !!y), 0)
  })
})

test_that("hl(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)

    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)

    expect_gte(probdist.hl(!!x, !!y), 0)
  })
})

test_that("bh(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)

    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)

    expect_gte(probdist.bh(!!x, !!y), 0)
  })
})

test_that("cd(x,y) is >= 0", {
  replicate(10, {
    x <- runif(10, min = 0:9, max = 1:10)
    x <- x / sum(x)
    
    y <- x + runif(10, min = 1, max = 2)
    y <- y / sum(y)
    
    expect_gte(probdist.cd(!!x, !!y), 0)
  })
})

################### symmetric

test_that("l1(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)
    
    y <- runif(10)
    y <- y / sum(y)
    
    expect_equal(
      probdist.l1(!!x, !!y),
      probdist.l1(!!y, !!x)
    )
  })
})

test_that("l2(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)

    y <- runif(10)
    y <- y / sum(y)

    expect_equal(
      probdist.l2(!!x, !!y),
      probdist.l2(!!y, !!x)
    )
  })
})

test_that("ry(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)

    y <- runif(10)
    y <- y / sum(y)

    expect_equal(
      probdist.ry(!!x, !!y),
      probdist.ry(!!y, !!x)
    )
  })
})

test_that("kl(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)

    y <- runif(10)
    y <- y / sum(y)

    expect_equal(probdist.kl(!!x, !!y), probdist.kl(!!y, !!x))
  })
})

test_that("tv(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)

    y <- runif(10)
    y <- y / sum(y)

    expect_equal(probdist.tv(!!x, !!y), probdist.tv(!!y, !!x))
  })
})

test_that("hl(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)

    y <- runif(10)
    y <- y / sum(y)

    expect_equal(probdist.hl(!!x, !!y), probdist.hl(!!y, !!x))
  })
})

test_that("bh(x,y) is symmetric", {
  replicate(10, {
    x <- runif(10)
    x <- x / sum(x)

    y <- runif(10)
    y <- y / sum(y)

    expect_equal(probdist.bh(!!x, !!y), probdist.bh(!!y, !!x))
  })
})

x <- c(0.3, 0.2, 0.5, 0)
y <- c(0.5, 0.4, 0.1, 0)

test_that("kl works for matching 0 prob", {
  expect_gte(probdist.kl(x, y), 0 )
})


test_that("ry works for matching 0 prob", {
  expect_gte(probdist.ry(x, y), 0 )
})

test_that("cd works for matching 0 prob", {
  expect_gte(probdist.cd(x, y), 0 )
})


test_that("bh works for matching 0 prob", {
  expect_gte(probdist.bh(x, y), 0 )
})

############## 


test_that("generate_xor_dataset", {
  DD <- generate_xor_dataset(n = 5, N = 99)
  expect_equal(dim(DD), c(99,6))
  expect_equal(colnames(DD)[1], "C")
})

test_that("generate_linear_dataset", {
  DD <- generate_linear_dataset(n = 5, N = 99)
  expect_equal(dim(DD), c(99,6))
  expect_equal(colnames(DD)[1], "C")
})

test_that("generate_random_dataset", {
  DD <- generate_random_dataset(n = 5, N = 99)
  expect_equal(dim(DD), c(99,5))
})

#####################

test_that("which_class (internal)",{
  D <- factor("D", c("A", "B", "C", "D", "E"))
  A <- factor("A", c("A", "B", "C", "D", "E"))
  expect_equal(stagedtrees:::which_class(c(-Inf, -10, -4, -1, -6),
                                         c("A", "B", "C", "D", "E")), !!D)
  expect_equal(stagedtrees:::which_class(c(-0.1, -10, -4, -1, -6),
                                         c("A", "B", "C", "D", "E")), !!A)
})

test_that("simple_clustering", {
  M <- matrix(runif(25), ncol = 5, dimnames = list(1:5,1:5))
  expect_is(stagedtrees:::simple_clustering(M), "list")
})
