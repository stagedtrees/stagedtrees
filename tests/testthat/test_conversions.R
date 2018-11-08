context("models conversion")


test_that("test that strt_ev_tree to staged_ev_tree works",{
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- strt_ev_tree(x = DD,order = c("B","A"), fit = TRUE, lambda = 1)
  sev <- staged_ev_tree(ev)
  expect_is(sev,"staged_ev_tree")
})



test_that("test that staged_ev_tree to strt_ev_tree works",{
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  sev <- staged_ev_tree(x = DD,order = c("B","A"), fit = TRUE, lambda = 1)
  ev <- strt_ev_tree(sev)
  expect_is(ev,"strt_ev_tree")
})

test_that("test that staged_ev_tree to strt_ev_tree keeps probabilities",{
  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  sev <- staged_ev_tree(x = DD,order = c("B","A"), fit = TRUE, lambda = 1)
  sev$prob$A$`1`[1]<-runif(1)
  sev$prob$A$`1`[2]<-1 - sev$prob$A$`1`[1]
  ev <- strt_ev_tree(sev)
  expect_true(all(ev$prob$A[1,] == sev$prob$A$`1` ) &&
                all(ev$prob$A[2,] == sev$prob$A$`1` )  )
})

test_that("test strt_ev_tree -> staged_ev_tree -> strt_ev_tree",{
  DD <- as.data.frame(sapply(1:6, function(i){
    return(as.factor(sample(c(0,1,2), size=100,
                            replace = TRUE)))
  }  ) )
  ev <- strt_ev_tree(DD, fit = TRUE, lambda = 1)
  sev <- staged_ev_tree(ev)
  evback <- strt_ev_tree(sev)
  for (k in 1:6){
    expect_equal(
      ev$prob[[k]],
      evback$prob[[k]]
    )  
  }
  
})



