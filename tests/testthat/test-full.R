test_that("test full model (NA)", {
  N <- sample(100:200, size = 1)
  ns <- sample(5:15, size = 3)
  DD <- as.data.frame(sapply(1:5, function(i) {
    return(as.factor(sample(
      c(0, 1, 2),
      size = N,
      replace = TRUE
    )))
  }))
  DD[sample(nrow(DD), ns[1]), 1] <- NA
  DD[sample(nrow(DD), ns[2]), 2] <- NA
  DD[sample(nrow(DD), ns[3]), 4] <- NA
  
  expect_silent(model <- full(DD))
  
  AA <- summary(model)
  
  expect_equal(sum(AA$stages.info$V1$sample.size), sum(!is.na(DD$V1)))
  expect_equal(sum(AA$stages.info$V2$sample.size), sum(!is.na(DD$V1) & 
                                                         !is.na(DD$V2)))
  expect_equal(sum(AA$stages.info$V3$sample.size), sum(!is.na(DD$V1) & 
                                                         !is.na(DD$V2) &
                                                         !is.na(DD$V3)))
  expect_equal(sum(AA$stages.info$V4$sample.size), sum(!is.na(DD$V1) & 
                                                         !is.na(DD$V2) &
                                                         !is.na(DD$V3) &
                                                         !is.na(DD$V4)))
  expect_equal(sum(AA$stages.info$V5$sample.size), sum(!is.na(DD$V1) & 
                                                         !is.na(DD$V2) &
                                                         !is.na(DD$V3) &
                                                         !is.na(DD$V4) &
                                                         !is.na(DD$V5)))
})

test_that("full does not crash when a variable has exactly one level (A-B1)", {
  df <- data.frame(A = c("a", "b"), B = c("x", "x"))
  expect_no_error(full(df, lambda = 1))
  m <- full(df, lambda = 1)
  expect_s3_class(m, "sevt")
  expect_true(has_ctables(m))
})

test_that("stages()[[character(0)]] does not crash (A-B4)", {
  m <- full(data.frame(A = factor(c("a", "b")), B = factor(c("x", "y"))), lambda = 1)
  expect_no_error(stages(m)[[character(0)]])
})
