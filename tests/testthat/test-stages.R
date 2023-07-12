## stages

test_that("stages function", {
  mod <- random_sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ))
  expect_equal(stages(mod)[["B"]], mod$stages$B)
  expect_equal(stages(mod)[["C"]], mod$stages$C)
})

test_that("stages function get all the stages", {
  mod <- random_sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ))
  expect_s3_class(stages(mod), "sevt.stgs")
  expect_identical(unclass(stages(mod))$B, mod$stages$B)
  expect_identical(unclass(stages(mod))$C, mod$stages$C)
})

test_that("stages()[vars]", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_identical(stages(mod)[c("A", "B", "C")],
                   list(A = "NA", B = c("1", "2"),
                        C = c("1", "2", "3", "4", "5", "6")))
})

test_that("stages()[var, context]", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_identical(stages(mod)["C", B = "bbb"], c("3", "6"))
})

test_that("stages()[var, context] fail if wrong context", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_error(stages(mod)["A", C = "c"]) # wrong order
  expect_error(stages(mod)["C", A = "c"]) # wrong levels
  expect_error(stages(mod)["C", A = "a", K = "o"]) # wrong var
})

test_that("stages()[var, context] fail if multiple var", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_error(stages(mod)[c("C", "B"), A = "a"]) # wrong levels
})


test_that("stages()[[path]]", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_identical(stages(mod)[[A = "a"]], "1")
  expect_identical(stages(mod)[[A = "a"]], get_stage(mod, "a"))
  expect_identical(stages(mod)[[A = "a", B = "bbb"]], "3")
  expect_identical(stages(mod)[[A = "a", B = "bbb"]],
                   get_stage(mod, path = c("a", "bbb")))
  expect_identical(stages(mod)[["a", "bbb"]],
                   get_stage(mod, path = c("a", "bbb")))
  expect_identical(stages(mod)[[c("a", "bbb")]],
                   get_stage(mod, path = c("a", "bbb")))
  expect_identical(stages(mod)[[c(A = "a", B = "bbb")]],
                   get_stage(mod, path = c("a", "bbb")))
})


test_that("stages()[[path]] fail if wrong path", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_error(stages(mod)[[c(L = "ll")]])
  expect_error(stages(mod)[[c(L = "ll", M = "mm")]])
})

test_that("stages()[var, context] <- value", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_silent(stages(mod)["B", A = "a"] <- "mystage1")
  expect_silent(stages(mod)["C", A = "a"] <- "mystage2")
  expect_silent(stages(mod)["C", A = "a", B = "bb"] <- "mystage3")
})


test_that("stages()[var, context] <- value fail if wrong context", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_error(stages(mod)["B", B = "bb"] <- "mystage1")
  expect_error(stages(mod)["C", B = "a"] <- "mystage2")
  expect_error(stages(mod)["C", A = "a", B = "bb", C = "c"] <- "mystage3")
})


test_that("stages()[[path]] <- value fail if wrong path", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_error(stages(mod)[[A = "ff"]] <- "mystage1")
  expect_error(stages(mod)[[A = "a", B = "bb", C = "cc"]] <- "mystage1")
})

test_that("stages()[[path]] <- value fail if wrong value", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_error(stages(mod)[[A = "aa"]] <- 1)
  expect_error(stages(mod)[[A = "aa", B = "bb"]] <- 1)
  expect_error(stages(mod)[[A = "aa"]] <- list(A = "aa"))
})

test_that("print stages", {
  mod <- sevt(list(
    "A" = c("a", "aa"),
    "B" = c("b", "bb", "bbb"),
    "C" = c("c", "cc")
  ), full = TRUE)
  expect_invisible(print(stages(mod)))
  expect_output(print(stages(mod)))
})
