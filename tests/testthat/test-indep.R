test_that("test indep model", {
  DD <- as.data.frame(sapply(1:4, function(i) {
    return(as.factor(sample(
      c(0, 1, 2),
      size = 100,
      replace = TRUE
    )))
  }))
  sev1 <- stagedtrees:::sevt_fit(sevt(DD), data = DD, lambda = 1)

  sev2 <- indep(DD, lambda = 1, join_unobserved = FALSE)

  expect_true(compare_stages(sev1, sev2))
})

test_that("indep.data.frame complete-cases matches indep.table with NAs (C6)", {
  set.seed(1)
  dat <- data.frame(
    A = c("a", "a", "b", "b", NA),
    B = c("x", "y", "x", "y", "x"),
    stringsAsFactors = FALSE
  )
  m_table <- indep(table(dat), join_unobserved = FALSE)
  m_df    <- indep(dat,        join_unobserved = FALSE)

  # logLikelihoods must agree (same complete-case counts)
  expect_equal(as.numeric(logLik(m_df)), as.numeric(logLik(m_table)),
               tolerance = 1e-8)

  # prob entries must be plain named numerics (no dim), matching table path
  expect_null(dim(m_df$prob$A[["1"]]))
  expect_null(dim(m_df$prob$B[["1"]]))
  expect_equal(m_df$prob$A[["1"]], m_table$prob$A[["1"]], tolerance = 1e-8)
})

test_that("indep.data.frame prob entries are plain named numeric, not table/array (C6)", {
  dat <- data.frame(A = c("a","b","a"), B = c("x","y","x"),
                    stringsAsFactors = FALSE)
  m <- indep(dat, join_unobserved = FALSE)
  expect_null(dim(m$prob$A[["1"]]))
  expect_null(dim(m$prob$B[["1"]]))
  expect_named(m$prob$A[["1"]])
  expect_named(m$prob$B[["1"]])
})

test_that("test indep model (NA)", {
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

  expect_silent(model <- indep(DD))

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
