context("plot base")

test_that("plot stratified event tree", {

  DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
  ev <- strt_ev_tree(x = DD,order = c("B","A"))
  expect_silent(plot(ev))

})

DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
ev <- staged_ev_tree(x = DD,order = c("B","A"))


test_that("plot staged event tree", {
  expect_silent(plot(ev))
})

test_that("plot should return NULL", {
  expect_null(plot(ev))
})


context("plot staged trees")

DD <- data.frame(A = as.factor(c(1,2,2,1)), B = as.factor(c("a","b","a","b")))
DD <- cbind(DD, generate_random_dataset(6, 4))
mod <- full(DD, fit = FALSE)

test_that("plot should accept col = 'stages' ", {
  expect_silent(plot(mod, col = "stages"))
})


test_that("plot should accept col = function() ", {
  expect_silent(plot(mod, col = function(s) return(rep(2, length(s)))))
})

