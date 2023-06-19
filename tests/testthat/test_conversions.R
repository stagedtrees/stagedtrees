context("as_sevt")

#manually create a parentslist
pl <- list("X" = list(parents = c(), values = c("a", "b")), 
           "Y" = list(parents = "X", values = c('1', "2")))
class(pl) <- "parentslist"

test_that("as.character.parentslist",{
  expect_visible(as.character(pl))
})

test_that("as.character.parentslist",{
  capture_output(expect_invisible(print(pl, )))
})

test_that("as_bn.parentslist",{
  expect_s3_class(as_bn(pl), "bn")
})

test_that("as_bn.sevt",{
  expect_s3_class(as_bn(as_sevt(pl)), "bn")
})

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

sev.i <- sevt(list(X = c("a", "b"), Y = c("1", "2"), 
                   Z = c('ee', 'ff', 'gg')))
sev.f <- sevt(list(X = c("a", "b"), Y = c("1", "2"),
                   Z = c('ee', 'ff', 'gg')), full = TRUE)

test_that("as_parentslist for sevt",{
  expect_silent(p.i <- as_parentslist(sev.i))
  expect_silent(p.f <- as_parentslist(sev.f))
  expect_null(p.i$X$parents)
  expect_length(p.i$Y$parents,0)
  expect_null(p.f$X$parents)
  expect_equal(p.f$Y$parents, "X")
})


test_that("as_parentslist for sevt (non DAG)",{
  sev <- sev.f
  sev$stages$Z <- c("s1", "s2", "s2", "s1")
  expect_message(p.i <- as_parentslist(sev))
  expect_silent(p.i <- as_parentslist(sev, silent = TRUE))
  expect_null(p.i$X$parents)
  expect_equal(p.i$Y$parents, "X")
  expect_setequal(p.i$Z$parents, c("X", "Y"))
})

test_that("as_parentslist for broken sevt should throw error",{
  sev <- sev.i
  sev$stages <- NULL
  expect_error(as_parentslist(sev))
  sev <- sev.i
  sev$tree <- NULL
  expect_error(as_parentslist(sev))
})

sev <- stages_fbhc(full(PhDArticles))
test_that("as_parentslist for more complex sevt",{
  expect_message(pl <- as_parentslist(sev))
  expect_null(pl$Articles$parents)
  expect_equal(pl$Prestige$parents, 
               c("Mentor", "Married", "Kids", "Gender", "Articles"))
})


test_that("as_adj_matrix.parentslist", {
  expect_message(pl <- as_parentslist(sev))
  expect_silent(p.i <- as_parentslist(sev.i))
  expect_silent(p.f <- as_parentslist(sev.f))
  expect_silent(A <- as_adj_matrix(pl))
  expect_equal(colnames(A), rownames(A))
  expect_equal(colnames(A),  names(sev$tree))
  AA <- matrix(0, nrow = 3, ncol = 3, 
               dimnames = list(names(sev.i$tree),names(sev.i$tree)))
  expect_identical(as_adj_matrix(p.i), AA)
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
