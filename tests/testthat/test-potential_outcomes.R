model0 <- model1 <- sevt(list(X1 = c("0", "1"),
                    X2 = c("0", "1"),
                    TT = c("a", "b"),
                    Y = c("-1", '1')), full = TRUE)

stages(model0)[["TT"]] <- c("1", "2", "2", "1")
stages(model0)[["Y"]] <- c("1", "1", "1", "2", "3", "2", "3", "3")



test_that("potential_outcomes return probabilities", {
  for (i in 1:10){
    model <- random_sevt(model0, q = 0)
    expect_silent(po <- potential_outcomes(model, outcome = "Y", treatment = "TT"))
    expect_lt(sum(abs(rowSums(po) - 1)), 1e-14)
  }
})


test_that("potential_outcomes return probabilities", {
  for (i in 1:10){
    model <- random_sevt(model1, q = 0.3)
    expect_silent(po <- potential_outcomes(model, outcome = "Y", treatment = "TT"))
    expect_lt(sum(abs(rowSums(po) - 1)), 1e-14)
  }
})
