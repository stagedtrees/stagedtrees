mod1 <- sevt(list("A" = c("a", "aa", "aaa"),
                  "B" = c("b", "bb", "bbb"),
                  "C" = c("c", "cc"),
                  "D" = c("d", "dd")), full = TRUE)
mod2 <- random_sevt(mod1)
mod3 <- random_sevt(mod1)
mod4 <- sevt(list("A" = c("a", "aa", "aaa"),
                  "B" = c("b", "bb", "bbb"),
                  "C" = c("c", "cc"),
                  "D" = c("d", "dd")), full = TRUE)
stages(mod4)["B", A = "a"] <- "2"


test_that("inclusion_stages works properly", {
  expect_silent(comparison <- inclusions_stages(mod1, mod4))
  expect_silent(comparison <- inclusions_stages(mod2, mod4))
  expect_silent(comparison <- inclusions_stages(mod3, mod4))
})

test_that("inclusion_stages works properly", {
  expect_silent(comparison <- inclusions_stages(mod2, mod3))
  for (i in 2:(sevt_nvar(mod1) - 1)) {
    expect_true(NROW(comparison[[i]]) > NROW(comparison[[i - 1]]))
  }
})

test_that("inclusion_stages works properly", {
  expect_silent(comparison <- inclusions_stages(mod1, mod2))
  for (i in 2:(sevt_nvar(mod1) - 1)) {
    expect_true(NROW(comparison[[i]]) > NROW(comparison[[i - 1]]))
    expect_in(comparison[[i]][[2]], expected =  c("<", "="))
  }
})

test_that("inclusion_stages works symmetrically", {
  expect_silent(comparison1 <- inclusions_stages(mod1, mod2))
  expect_silent(comparison2 <- inclusions_stages(mod2, mod1))
  for (i in 1:(sevt_nvar(mod1) - 1)) {
    expect_true(NROW(comparison1[[i]]) == NROW(comparison2[[i]]))
    expect_in(comparison1[[i]][[2]], expected =  c("<", "="))
    expect_in(comparison2[[i]][[2]], expected =  c(">", "="))

  }
})
