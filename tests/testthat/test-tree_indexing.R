test_that("find_stage find the correct stage", {
  DD <-
    data.frame(
      A = as.factor(c(1, 2, 2, 1)),
      B = as.factor(c("a", "b", "a", "b"))
    )
  sevt <- sevt(DD, order = c("B", "A"), full = TRUE)
  stg1 <- find_stage(sevt, path = c("a"))
  stg2 <- find_stage(sevt, path = c("b"))
  expect_equal(c(stg1, stg2), c("1", "2"))
})

test_that("find_stage find the correct stage (scrambled stages)", {
  DD <-
    data.frame(
      A = as.factor(c(1, 2, 2, 1)),
      B = as.factor(c("a", "b", "a", "b")),
      C = as.factor(c("1", "2", "3", "3"))
    )
  sevt <- sevt(DD, order = c("B", "A", "C"), full = TRUE)
  sevt$stages <- sevt$stages[2:1]
  stg1 <- find_stage(sevt, path = c("a"))
  stg2 <- find_stage(sevt, path = c("b"))
  stg3 <- find_stage(sevt, path = c("a", "1"))
  stg4 <- find_stage(sevt, path = c("b", "1"))
  expect_equal(c(stg1, stg2, stg3, stg4), c("1", "2", "1", "3"))
})

test_that("find_stage find the correct stage (scrambled stages 2)", {
  DD <- generate_linear_dataset(p = 6, n = 10)
  sevt <- sevt(DD, full = TRUE)
  replicate(5, {
    pth <- sample(c("1", "-1"), size = 4, replace = TRUE)
    stg <- find_stage(sevt, pth)
    sevt$stages <- sevt$stages[sample(seq_along(sevt$stages))]
    expect_equal(stg, find_stage(sevt, pth))
  })
})
