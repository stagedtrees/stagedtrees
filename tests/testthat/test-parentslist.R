# manually create a parentslist
pl <- list(
  "X" = list(parents = c(), values = c("a", "b")),
  "Y" = list(parents = "X", values = c("1", "2"))
)
class(pl) <- "parentslist"

sev.i <- sevt(list(
  X = c("a", "b"), Y = c("1", "2"),
  Z = c("ee", "ff", "gg")
))
sev.f <- sevt(list(
  X = c("a", "b"), Y = c("1", "2"),
  Z = c("ee", "ff", "gg")
), full = TRUE)


test_that("as.character.parentslist", {
  expect_visible(as.character(pl))
})

test_that("as.character.parentslist", {
  capture_output(expect_invisible(print(pl, )))
})


test_that("as_parentslist for sevt", {
  expect_silent(p.i <- as_parentslist(sev.i))
  expect_silent(p.f <- as_parentslist(sev.f))
  expect_null(p.i$X$parents)
  expect_length(p.i$Y$parents, 0)
  expect_null(p.f$X$parents)
  expect_equal(p.f$Y$parents, "X")
})


test_that("as_parentslist for sevt (non DAG)", {
  sev <- sev.f
  sev$stages$Z <- c("s1", "s2", "s2", "s1")
  expect_message(p.i <- as_parentslist(sev))
  expect_silent(p.i <- as_parentslist(sev, silent = TRUE))
  expect_null(p.i$X$parents)
  expect_equal(p.i$Y$parents, "X")
  expect_setequal(p.i$Z$parents, c("X", "Y"))
})

test_that("as_parentslist for broken sevt should throw error", {
  sev <- sev.i
  sev$stages <- NULL
  expect_error(as_parentslist(sev))
  sev <- sev.i
  sev$tree <- NULL
  expect_error(as_parentslist(sev))
})

sev <- stages_fbhc(full(PhDArticles))
test_that("as_parentslist for more complex sevt", {
  expect_message(pl <- as_parentslist(sev))
  expect_null(pl$Articles$parents)
  expect_equal(
    pl$Prestige$parents,
    c("Mentor", "Married", "Kids", "Gender", "Articles")
  )
})
