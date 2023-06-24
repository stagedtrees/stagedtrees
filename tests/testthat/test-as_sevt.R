# manually create a parentslist
pl <- list(
  "X" = list(parents = c(), values = c("a", "b")),
  "Y" = list(parents = "X", values = c("1", "2"))
)
class(pl) <- "parentslist"

test_that("as_sevt for parentslist", {
  expect_silent(sev <- as_sevt(pl))
  expect_equal(names(sev$tree), c("X", "Y"))
  expect_equal(sev$tree$X, c("a", "b"))
  expect_equal(sev$tree$Y, c("1", "2"))
  expect_equal(sev$stages$Y, c("1", "2"))
})

test_that("as_sevt for parentslist with values input", {
  expect_silent(sev <- as_sevt(pl, values = list(Y = c("ff", "gg"))))
  expect_equal(names(sev$tree), c("X", "Y"))
  expect_equal(sev$tree$X, c("a", "b"))
  expect_equal(sev$tree$Y, c("ff", "gg"))
  expect_equal(sev$stages$Y, c("1", "2"))
})

test_that("as_sevt for parentslist should throw warning if no values", {
  pl$Y$values <- NULL
  expect_warning(sev <- as_sevt(pl))
  expect_equal(names(sev$tree), c("X", "Y"))
  expect_equal(sev$tree$X, c("a", "b"))
  expect_equal(sev$tree$Y, c(0, 1))
  expect_equal(sev$stages$Y, c("1", "2"))
})


test_that("as_sevt.bn", {
  bn <- bnlearn::hc(PhDArticles)
  expect_silent(as_sevt(bn, values = lapply(PhDArticles, levels)))
})

test_that("as_sevt.bn.fit", {
  bn <- bnlearn::hc(PhDArticles)
  bn <- bnlearn::bn.fit(bn, PhDArticles)
  expect_silent(as_sevt(bn, values = lapply(PhDArticles, levels)))
})
