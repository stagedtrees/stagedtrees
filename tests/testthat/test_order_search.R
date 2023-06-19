context("sevt_add")

DD <- generate_linear_dataset(5, 100)
DDna <- DD
DDna[sample(100, 10), sample(6,1)] <- NA
DDna[sample(100, 10), sample(6,1)] <- NA
DDna[sample(100, 10), sample(6,1)] <- NA
levels(DD$C) <- c("a", "b")
levels(DD$X3) <- c("qqqq", "pppp")
sev <- full(DD, order = c("X1", "C", "X2") )

test_that("sevt_add add the right variable", {
  sev1 <- sevt_add(sev, "X4", data = DD)
  expect_equal(names(sev1$tree)[4], "X4")
  
  sev2 <- sevt_add(sev, "X3", data = DD)
  expect_equal(names(sev2$tree)[4], "X3")
})

test_that("sevt_add add throw error for non-existing variable", {
  expect_error(sevt_add(sev, "YY", data = DD))
})

test_that("logLik is correct after sevt_add",{
  sev1 <- sevt_add(sev, "X3", data = DD)
  sev1a <- full(DD, order = c("X1", "C", "X2", "X3"))
  expect_equal(logLik(sev1), logLik(sev1a))
})


test_that("logLik is correct after sevt_add (NAs)",{
  sevna <- full(DDna, order = c("X1", "C", "X2") )
  sev1 <- sevt_add(sevna, "X3", data = DDna)
  sev1a <- full(DDna, order = c("X1", "C", "X2", "X3"))
  expect_equal(logLik(sev1), logLik(sev1a))
})

test_that("sevt_add throw error for wrong data class",{
  expect_error(sevt_add(sev, "X3", data = NULL))
  expect_error(sevt_add(sev, "X3", data = NA))
  expect_error(sevt_add(sev, "X3", data = c(1:100)))
})

## not testing with stages_hc because slow
algs <- list(
  stages_bhc,
  stages_bj,
  stages_fbhc
)

context("search_greedy")

test_that("search greedy for data.frames", {
  for (alg in algs){
    expect_is(search_greedy(data = DD, alg = alg), class = "sevt")
  }
})

DD.t <- table(DD)
test_that("search greedy for table", {
  for (alg in algs){
    expect_is(search_greedy(data = DD.t, alg = alg), class = "sevt")
  }
})

context("search_best")

test_that("search_best for data.frames", {
  expect_is(search_best(data = DD, alg = stages_fbhc), class = "sevt")
})


test_that("search_best for data.frames with NAs", {

  expect_is(search_best(data = DD, alg = stages_fbhc), class = "sevt")
})

DD.t <- table(DD)
test_that("search_best for table", {
  expect_is(search_best(data = DD.t, alg = stages_fbhc), class = "sevt")
})