sev_0 <- sevt(list(
  "X1" = c("-1", "+1"),
  "AA" = c("aa", "bb", "cc"),
  "B.B" = c("0", "1"),
  "X20" = c("23", "56")
))

sev_2 <- random_sevt(list(
  "X1" = c("-1", "+1"),
  "AA" = c("aa", "bb", "cc"),
  "B.B" = c("0", "1"),
  "X20" = c("23", "56")
))

sev_1 <- sev_2
sev_1$prob <- NULL

sev_3 <- sevt_fit(sev_2, data = sample_from(sev_2, nsim = 100), lambda = 1)

test_that("summary.sevt no ctables no prob", {
  expect_visible(summary(sev_1))
  expect_silent(summary(sev_1))
  expect_s3_class(summary(sev_1), class = "summary.sevt")
})

test_that("summary.sevt only prob", {
  expect_visible(summary(sev_2))
  expect_silent(summary(sev_2))
  expect_s3_class(summary(sev_2), class = "summary.sevt")
})

test_that("summary.sevt fitted", {
  expect_visible(summary(sev_3))
  expect_silent(summary(sev_3))
  expect_s3_class(summary(sev_3), class = "summary.sevt")
})

test_that("summary.sevt for subtree", {
  expect_visible(summary(subtree(sev_1, c("-1", "aa"))))
  expect_silent(summary(subtree(sev_1, c("-1", "aa"))))

  expect_visible(summary(subtree(sev_2, c("-1", "aa"))))
  expect_silent(summary(subtree(sev_2, c("-1", "aa"))))

  expect_visible(summary(subtree(sev_3, c("-1", "aa"))))
  expect_silent(summary(subtree(sev_3, c("-1", "aa"))))
})

test_that("summary.sevt for indep", {
  expect_visible(summary(sev_0))
  summary(summary(sev_0))
})

test_that("summary.sevt is printed", {
  expect_output(print(summary(sev_1)))
  expect_output(print(summary(sev_2)))
  expect_output(print(summary(sev_3)))
  
  
})