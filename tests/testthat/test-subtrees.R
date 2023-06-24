sev <- random_sevt(list(c("-1", "1"),
                        c("a", "aa"),
                        c("BB", "CC"),
                        c("DD", 'dd')))
data <- sample_from(sev, 100)
sev2 <- sevt_fit(sev, data, lambda = 0)

test_that("subtree", {
  expect_silent(sub <- subtree(sev, c("1", "aa")))
  expect_silent(plot(sub))
})

test_that("sevt_fit on subtree", {
  sub <- subtree(sev, c("1", "aa"))
  expect_silent(sevt_fit(sub, data = data, lambda = 0))
})

test_that("depsubtree", {
  expect_silent(sub <- depsubtree(sev, "V3"))
  expect_silent(plot(sub))
})

test_that("depsubtree (other stages)", {
  expect_silent(sub <- depsubtree(sev, "V3", other_stages = "indep"))
  expect_silent(plot(sub))
})

test_that("sevt_fit depsubtree", {
  expect_silent(sub <- depsubtree(sev, "V3", other_stages = "indep"))
  expect_silent(sevt_fit(sub, data = data, lambda = 0))
})

