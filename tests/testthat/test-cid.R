modf <- full(Titanic)
mod1 <- stages_bj(modf)
modi <- indep(Titanic)
mod2 <- stages_bj(full(Titanic, order = names(modf$tree)[4:1]))

test_that("cid with full model is 0", {
  expect_silent(cid.res <- cid(mod1, modf))
  expect_equal(cid.res$cid, 0)
})

test_that("cid wrt indep model is 0", {
  expect_silent(cid.res <- cid(modi, mod1))
  expect_equal(cid.res$cid, 0)
})

test_that("cid(mod,mod) == 0", {
  expect_silent(cid.res <- cid(mod1, mod1))
  expect_equal(cid.res$cid, 0)
})

test_that("cid >= 0", {
  expect_silent(cid.res <- cid(mod1, mod2))
  expect_gte(cid.res$cid, 0)
})
