# context("plot functions")
# 
# test_that("plot stratified event tree", {
# 
#   DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
#   ev <- strt_ev_tree(x = DD,order = c("B","A"))
#   expect_silent(plot(ev))
# 
# })
# 
# test_that("plot staged event tree", {
# 
#   DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
#   ev <- staged_ev_tree(x = DD,order = c("B","A"))
#   expect_silent(plot(ev))
# 
# })
