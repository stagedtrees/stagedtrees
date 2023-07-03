test_that("sevt_simplify simplify", {
  tree <- sevt(list("A" = c("1","2"), "B" = c("1","2"),
                    "C" = c("1", "2", "3"), "D" = c("1","2")))
  tree$stages$D[c(2,4,6,10,11,12)] <- "2"
  expect_silent(simpl <- sevt_simplify(tree))
  ceg_s <- ceg(simpl)
  expect_identical(stndnaming(simpl)$stages, ceg_s$positions[-1])
})

test_that("sevt_simplify simplify (fitted model)", {
  tree <- random_sevt(list("A" = c("1","2"), "B" = c("1","2"),
                    "C" = c("1", "2", "3"), "D" = c("1","2")))
  tree <- sevt_fit(tree, sample_from(tree, 100), lambda = 1)
  expect_silent(simpl <- sevt_simplify(tree))
  ceg_s <- ceg(simpl)
  expect_identical(stndnaming(simpl)$stages, ceg_s$positions[-1])
})
