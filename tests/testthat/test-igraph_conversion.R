model <- sevt(list(
  A = c("1a", "2a"), B = c("1b", "2b", "3b"),
  C = c("1c", "2c"), X = c("xx", "yy")
))

model$stages$B <- c("aaaa", "bbbb")
model$stages$C <- c("1", "1", "1", "2", "2", "aaaa")

model.ceg <- ceg(model)


test_that("get_edges", {
  edg <- get_edges(model)
  expect_equal(nrow(edg), 2 + 6 + 12 + 24)
  expect_equal(colnames(edg)[1:2], c("from", "to"))
})
test_that("get_vertices", {
  vert <- get_vertices(model)
  expect_equal(nrow(vert), 1 + 2 + 6 + 12 + 24)
})

test_that("get_vertices and get_edges are compatible I ", {
  vert <- get_vertices(model, ignore = FALSE)
  edg <- get_edges(model, ignore = FALSE)
  expect_setequal(vert$name, unique(c(edg$from, edg$to)))
})

test_that("get_vertices and get_edges are compatible II (with ignore)", {
  vert <- get_vertices(model, ignore = "1")
  edg <- get_edges(model, ignore = "1")
  expect_setequal(vert$name, unique(c(edg$from, edg$to)))
})

test_that("get_edges", {
  edg <- get_edges(ceg(model))
  expect_equal(nrow(edg), 16)
  expect_equal(colnames(edg)[1:2], c("from", "to"))
})
test_that("get_vertices", {
  vert <- get_vertices(ceg(model))
  expect_equal(nrow(vert), 8)
})

test_that("get_vertices and get_edges are compatible", {
  vert <- get_vertices(ceg(model), ignore = FALSE)
  edg <- get_edges(ceg(model), ignore = FALSE)
  expect_setequal(vert$name, unique(c(edg$from, edg$to)))
})
