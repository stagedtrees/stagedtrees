ds <- c(
  probdist.l1, probdist.l2, probdist.ry,
  probdist.kl, probdist.tv,
  probdist.hl, probdist.bh, probdist.cd
)

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

# testing symmetry symmetric

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
  expect_gte(probdist.kl(x, y), 0)
})


test_that("ry works for matching 0 prob", {
  expect_gte(probdist.ry(x, y), 0)
})

test_that("cd works for matching 0 prob", {
  expect_gte(probdist.cd(x, y), 0)
})


test_that("bh works for matching 0 prob", {
  expect_gte(probdist.bh(x, y), 0)
})
