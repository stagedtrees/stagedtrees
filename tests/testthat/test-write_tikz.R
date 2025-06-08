model <- sevt(list(
  A = c("1a", "2a"), B = c("1b", "2b", "3b"),
  C = c("1c", "2c"), X = c("xx", "yy")
))

model$stages$B <- c("aaaa", "bbbb")
model$stages$C <- c("1", "1", "1", "2", "2", "aaaa")

model.ceg <- ceg(model)

test_that("write_tikz", {
  expect_output(write_tikz(model))
})


test_that("write_tikz with layout", {
  layout <- igraph::layout_as_tree(as_igraph(model))
  expect_output(write_tikz(model, layout = layout, normalize_layout = FALSE))
})


test_that("write_tikz with layout and no ignore", {
  layout <- igraph::layout_as_tree(as_igraph(model, ignore = FALSE))
  expect_output(write_tikz(model, layout = layout, normalize_layout = FALSE, ignore = FALSE))
})

test_that("write_tikz with stages col", {
  model <- stndnaming(model, uniq = TRUE)
  expect_output(write_tikz(model, col = "stages"))
})

test_that("write_tikz works for ceg", {
  expect_output(write_tikz(ceg(model)))
})

test_that("write_tikz works with partial col", {
  model$stages$C <- paste0(1:6)
  expect_output(write_tikz(model, col = "classic"))
})
