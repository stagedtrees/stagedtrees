model <- full(PhDArticles)

test_that("has_prob returns TRUE for fitted model", {
  expect_true(has_prob(model))
})

test_that("has_prob returns FALSE if no prob field", {
  model_ <- model
  model_$prob <- NULL
  expect_false(has_prob(model_))
})

test_that("has_prob returns FALSE if one var missing from probabilities", {
  model_ <- model
  model_$prob$Kids <- NULL
  expect_false(has_prob(model_))
})


test_that("has_prob returns FALSE if one var has wrong length prob", {
  model_ <- model
  model_$prob$Kids[["4"]] <- c(0.1, 0.4, 0.5)
  expect_false(has_prob(model_))
})

test_that("has_ctables returns FALSE if no ctables", {
  model_ <- model
  model_$ctables <- NULL
  expect_false(has_ctables(model_))
})

test_that("is_fitted_sevt returns FALSE if no ctables or no prob", {
  model_ <- model
  model_$ctables <- NULL
  expect_false(is_fitted_sevt(model_))
  model_ <- model
  model_$prob <- NULL
  expect_false(is_fitted_sevt(model_))
})

test_that("check_sevt fails if no sevt class", {
  model_ <- model
  class(model_) <- "asdaf"
  expect_error(check_sevt(model_))
})

test_that("check_sevt fails if no object", {
  model_ <- 1
  expect_error(check_sevt(model_))
  model_ <- c("aaa")
  expect_error(check_sevt(model_))
})

test_that("check_sevt detects no tree", {
  model_ <- model
  model_$tree <- NULL
  expect_error(check_sevt(model_))
})

test_that("check_sevt detects no stages", {
  model_ <- model
  model_$stages <- NULL
  expect_error(check_sevt(model_))
})


test_that("check_sevt detects no stages (1 var model)", {
  model_ <- sevt(list(A = c("a", "aa")))
  model_$stages <- NULL
  expect_error(check_sevt(model_))
})

test_that("check_sevt detects no stages (2 var model)", {
  model_ <- sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb")
  ))
  expect_silent(check_sevt(model))
  model_$stages <- NULL
  expect_error(check_sevt(model_))
})

test_that("check_sevt detects no stages (1 var model)", {
  model_ <- sevt(list(A = c("a", "aa")))
  expect_silent(check_sevt(model))
  model_$stages <- NULL
  expect_error(check_sevt(model_))
})

test_that("check_tree fails if no named tree", {
  tree <- model$tree
  expect_silent(check_tree(tree, arg = "a", call = match.call()))
  names(tree) <- NULL
  expect_error(check_tree(tree, arg = "a", call = match.call()))
})

test_that("check_stages fails if no named stages", {
  model_ <- model
  expect_silent(check_stages(model_, arg = "a", call = match.call()))
  names(model_$stages) <- NULL
  expect_error(check_stages(model_, arg = "a", call = match.call()))
})

test_that("check_sevt_prob fails if no prob", {
  model_ <- model
  expect_silent(check_sevt_prob(model_))
  model_$prob <- NULL
  expect_error(check_sevt_prob(model_))
})

test_that("check_sevt_ctables fails if no ctables", {
  model_ <- model
  expect_silent(check_sevt_ctables(model_))
  model_$ctables <- NULL
  expect_error(check_sevt_ctables(model_))
})

test_that("check_sevt_prob and check_sevt_ctables fail if no fitted", {
  model_ <- sevt(list(A = c("a", "aa"), B = c("b", "bb")))
  expect_error(check_sevt_prob(model_))
  expect_error(check_sevt_ctables(model_))
})

test_that("check_sevt_prob passes if model has prob (2 var)", {
  model_ <- random_sevt(list(A = c("a", "aa"), B = c("b", "bb")))
  expect_silent(check_sevt_prob(model_))
})

test_that("check_sevt_prob passes if model has prob (1 var)", {
  model_ <- random_sevt(list(A = c("a", "aa")))
  expect_silent(check_sevt_prob(model_))
})


test_that("check_same_tree passes for same object", {
  expect_silent(check_same_tree(model, model))
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  expect_silent(check_same_tree(mod, mod))
})

test_that("check_same_tree passes if same tree", {
  mod1 <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  mod2 <- random_sevt(mod1)
  expect_silent(check_same_tree(mod1, mod2))
})

test_that("check_same_tree fails if different order", {
  mod1 <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  mod2 <- random_sevt(list(
    A = c("a", "aa"),
    C = c("c", "cc", "ccc"),
    B = c("b", "bb", "bbb"),
    D = c("d", "dd")
  ))
  expect_error(check_same_tree(mod1, mod2))
})

test_that("check_var_in passes for all variables in object", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  for (v in sevt_varnames(mod)) {
    expect_silent(check_var_in(v, mod))
  }
})

test_that("check_var_in fails for variables not in object", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  for (v in c("AA", "BB", "CC", "DD")) {
    expect_error(check_var_in(v, mod))
  }
})

test_that("check_scope passes for all variables in object", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  for (v in sevt_varnames(mod)) {
    expect_silent(check_scope(v, mod))
  }
})

test_that("check_scope fails for variables not in object", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  for (v in c("AA", "BB", "CC", "DD")) {
    scope <- c(v, sevt_varnames(mod))
    expect_error(check_scope(v, mod))
    expect_error(check_scope(scope, mod))
  }
})

test_that("check_scope works for tree as named list", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  scope <- sample(sevt_varnames(mod), size = 2)
  expect_silent(check_scope(scope, mod$tree))
  expect_silent(check_scope(scope, mod$tree))
})

test_that("check_path works for good paths", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  paths <- expand.grid(mod$tree[c(1, 2, 3)])
  for (i in nrow(paths)) {
    expect_silent(check_path(paths[i, ], mod$tree))
  }
})

test_that("check_path fails if path too long", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  expect_error(check_path(
    c(A = "aa", A = "a", B = "bb", C = "c", D = "dd"),
    mod$tree
  ))
})

test_that("check_context fails if context too long", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  expect_error(check_context(
    c(A = "aa", A = "a", B = "bb", C = "c", D = "dd"),
    "D", mod$tree
  ))
})


test_that("check_context works for good xontext", {
  mod <- random_sevt(list(
    A = c("a", "aa"),
    B = c("b", "bb", "bbb"),
    C = c("c", "cc", "ccc"),
    D = c("d", "dd")
  ))
  paths <- expand.grid(mod$tree[c(1, 2, 3)])
  for (i in nrow(paths)) {
    expect_silent(check_context(paths[i, ], "D", mod$tree))
  }
})
