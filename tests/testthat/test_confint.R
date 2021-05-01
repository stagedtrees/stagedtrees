context("confidence intervals function")

sides <- c("two.sided", "left", "right")
methods <- c("goodman", "wald", "waldcc", "wilson")
level <- c(0, 0.2, 0.9, 1)
check_ci <- function(ci){
  expect_type(ci, "double")
  expect_true(all(ci >= 0))
  expect_true(all(ci <= 1))
  expect_true(all(ci[,1] <= ci[,2]))
}
test_that("confint.sevt - full", {
  m1 <- indep(PhDArticles)
  ci <- confint(m1)
  for (sid in sides){
    for (meth in methods){
      for (l in level){
        ci <- confint(m1, sides = sid, method = meth)
        check_ci(ci)
      }
    }
  }
})

test_that("confint.sevt - indep", {
  m1 <- indep(PhDArticles)
  ci <- confint(m1)
  for (sid in sides){
    for (meth in methods){
      for (l in level){
        ci <- confint(m1, sides = sid, method = meth)
        check_ci(ci)
      }
    }
  }
})

test_that("confint.sevt - numeric parm", {
  m1 <- indep(PhDArticles)
  for (sid in sides){
    for (meth in methods){
      for (l in level){
        ci <- confint(m1, parm = c(2,1), sides = sid, method = meth)
        check_ci(ci)
      }
    }
  }
})

test_that("confint.sevt - 1 var model", {
  m1 <- indep(PhDArticles, order = c("Articles"))
  for (sid in sides){
    for (meth in methods){
      for (l in level){
        ci <- confint(m1, parm = c(2,1), sides = sid, method = meth)
        check_ci(ci)
      }
    }
  }
})




