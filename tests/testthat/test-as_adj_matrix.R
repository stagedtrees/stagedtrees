#manually create a parentslist
pl <- list("X" = list(parents = c(), values = c("a", "b")), 
           "Y" = list(parents = "X", values = c('1', "2")))
class(pl) <- "parentslist"

sev.i <- sevt(list(
  X = c("a", "b"), Y = c("1", "2"),
  Z = c("ee", "ff", "gg")
))
sev.f <- sevt(list(
  X = c("a", "b"), Y = c("1", "2"),
  Z = c("ee", "ff", "gg")
), full = TRUE)

sev <- join_stages(sev.f, "Z", "2", "4")

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
