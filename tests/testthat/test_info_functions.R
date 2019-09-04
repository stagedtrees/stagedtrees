context("info functions")

test_that("test that nvar.sevt return number of var", {
  ev <-
    staged_ev_tree.list(x = list(
      A = c(1, 2),
      B = c("x", "y"),
      C = c("3", "4")
    ))
  expect_equal(nvar.sevt(ev), 3)
  
  ev <- staged_ev_tree.list(x = list(
    A = c(1, 2)
  )) 
  expect_equal(nvar.sevt(ev), 1)
  
})

test_that("test that summary.sevt works properly with not-increasing numbering of stages", {
  
  model <- bhc.sevt(full(PhDArticles, lambda = 1))
  summ <- summary(model)
  
  expect_equal(NROW(summ$stages.info$Mentor), length(unique(model$stages$Mentor)))
  
  expect_equal(as.numeric(summ$stages.info$Mentor[summ$stages.info$Mentor$stage == "6", -c(1:3)]), as.numeric(model$prob$Mentor$`6`))
  
  expect_equal(summ$stages.info$Married[summ$stages.info$Married$stage == "10", 3], attr(model$prob$Married$`10`, "n"))
  
})
