
model <- sevt(list(A = c("1a", "2a"), B = c("1b", "2b", "3b"), 
                   C = c("1c", "2c"), X = c("xx", "yy")))

model$stages$B  <- c("aaaa", "bbbb")
model$stages$C <- c("1" ,"1", "1", "2", "2", "aaaa")

model.ceg <- ceg(model)

context("graph conversions for sevt")

test_that("sevt2edges",{
  edg <- get_edges(model)
  expect_equal(nrow(edg), 2 + 6 + 12 + 24)
  expect_equal(colnames(edg)[1:2], c("from", "to"))
})


test_that("sevt2vert",{
  vert <- get_vertices(model)
  expect_equal(nrow(vert), 1 + 2 + 6 + 12 + 24)
})

test_that("sevt2vert and sevt2edges are compatible I ",{
  vert <- get_vertices(model, ignore = FALSE)
  edg <- get_edges(model, ignore = FALSE)
  expect_setequal(vert$name, unique(c(edg$from, edg$to)))
})

test_that("sevt2vert and sevt2edges are compatible II (with ignore)",{
  vert <- get_vertices(model, ignore = "1")
  edg <- get_edges(model, ignore = "1")
  expect_setequal(vert$name, unique(c(edg$from, edg$to)))
})




