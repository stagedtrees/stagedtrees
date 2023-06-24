#manually create a parentslist
pl <- list("X" = list(parents = c(), values = c("a", "b")), 
           "Y" = list(parents = "X", values = c('1', "2")))
class(pl) <- "parentslist"

test_that("as_bn.parentslist",{
  expect_s3_class(as_bn(pl), "bn")
})

test_that("as_bn.sevt",{
  expect_s3_class(as_bn(as_sevt(pl)), "bn")
})