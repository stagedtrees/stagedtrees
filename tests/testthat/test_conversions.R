context("as_sevt")

#manually create a parentslist
pl <- list("X" = list(parents = c(), values = c("a", "b")), 
           "Y" = list(parents = "X", values = c('1', "2")))
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

sev.i <- sevt(list(X = c("a", "b"), Y = c("1", "2")))
sev.f <- sevt(list(X = c("a", "b"), Y = c("1", "2")), full = TRUE)

test_that("as_parentslist for sevt",{
  expect_silent(p.i <- as_parentslist(sev.i))
  expect_silent(p.f <- as_parentslist(sev.f))
  expect_null(p.i$X$parents)
  expect_length(p.i$Y$parents,0)
  expect_null(p.f$X$parents)
  expect_equal(p.f$Y$parents, "X")
})

test_that("as_parentslist for sevt",{
  expect_silent(p.i <- as_parentslist(sev.i))
  expect_silent(p.f <- as_parentslist(sev.f))
  expect_null(p.i$X$parents)
  expect_length(p.i$Y$parents,0)
  expect_null(p.f$X$parents)
  expect_equal(p.f$Y$parents, "X")
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
  AA <- matrix(c(0,0,0,0), nrow = 2, dimnames = list(names(sev.i$tree),names(sev.i$tree)))
  expect_identical(as_adj_matrix(p.i), AA)
})

