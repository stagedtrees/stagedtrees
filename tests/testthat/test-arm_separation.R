test_that("arm_index splits the situations by treatment value", {
  m <- full(Titanic, lambda = 1)  # Class, Sex, Age, Survived
  ## the treatment just before the outcome alternates at every situation
  a <- stagedtrees:::arm_index(m, "Age", "Survived")
  expect_equal(a$k, 2)
  expect_equal(a$arm, rep(c(1, 2), 8))
  ## two variables apart, it changes every 2 * 2 situations
  b <- stagedtrees:::arm_index(m, "Class", "Survived")
  expect_equal(b$k, 4)
  expect_equal(b$arm, rep(1:4, each = 4))
  expect_equal(b$context, rep(0:3, 4))
})

test_that("arm_separation finds the contexts whose arms share a stage", {
  m <- full(Titanic, lambda = 1)
  ## put both Age arms of (1st, Male) in one stage: a zero effect there
  s <- stages(m)[["Survived"]]
  s[1:2] <- "tied"
  stages(m)["Survived"] <- s

  viol <- arm_separation(m, "Age", "Survived")
  expect_equal(nrow(viol), 1)
  expect_equal(viol$Class, "1st")
  expect_equal(viol$Sex, "Male")
  expect_equal(viol$Age, "Child, Adult")
  expect_equal(viol$stage, "tied")
  expect_gt(viol$context_probability, 0)
  expect_named(viol, c("Class", "Sex", "Age", "stage", "context_probability"))
})

test_that("arm_separation reports nothing when the arms are separated", {
  m <- full(Titanic, lambda = 1)
  expect_equal(nrow(arm_separation(m, "Age", "Survived")), 0)
  expect_output(print(arm_separation(m, "Age", "Survived")), "separated in every context")
})

test_that("arm_separation looks past the variables between treatment and outcome", {
  m <- full(Titanic, lambda = 1)
  ## situations 1 and 5 of Survived share (Sex, Age) and differ in Class
  s <- stages(m)[["Survived"]]
  s[c(1, 5)] <- "tied"
  stages(m)["Survived"] <- s
  viol <- arm_separation(m, "Class", "Survived")
  expect_equal(nrow(viol), 1)
  expect_equal(viol$Class, "1st, 2nd")
  ## and those two situations do not differ only in Age
  expect_equal(nrow(arm_separation(m, "Age", "Survived")), 0)
})

test_that("separate_arms splits only the stages which tie two arms", {
  m <- full(Titanic, lambda = 1)
  s <- stages(m)[["Survived"]]
  s[1:2] <- "tied"
  stages(m)["Survived"] <- s
  n_before <- length(unique(stages(m)[["Survived"]]))

  ms <- separate_arms(m, "Age", "Survived")
  expect_equal(nrow(arm_separation(ms, "Age", "Survived")), 0)
  expect_equal(length(unique(stages(ms)[["Survived"]])), n_before + 1)
})

test_that("separate_arms leaves a separated staging exactly as it is", {
  m <- full(Titanic, lambda = 1)
  ## one stage holding different arms of different contexts states nothing
  ## about an effect, so it must not be split
  s <- stages(m)[["Survived"]]
  s[c(1, 4)] <- "spanning"
  stages(m)["Survived"] <- s
  expect_equal(nrow(arm_separation(m, "Age", "Survived")), 0)

  ms <- separate_arms(m, "Age", "Survived")
  expect_identical(stages(ms)[["Survived"]], stages(m)[["Survived"]])
  expect_equal(as.numeric(logLik(ms)), as.numeric(logLik(m)))
})

test_that("ps_stratify separates the arms by construction", {
  set.seed(4)
  d <- data.frame(
    X = factor(sample(c("0", "1"), 500, TRUE)),
    TT = factor(sample(c("a", "b"), 500, TRUE)),
    Y = factor(sample(c("n", "y"), 500, TRUE))
  )
  m <- stages_bhc(full(d, lambda = 1))
  ps <- ps_stratify(m, "TT", "Y")
  expect_equal(nrow(arm_separation(ps, "TT", "Y")), 0)
})

test_that("arm_separation and separate_arms respect ignore", {
  set.seed(3)
  d <- data.frame(
    X = factor(sample(c("0", "1"), 300, TRUE)),
    TT = factor(sample(c("a", "b"), 300, TRUE)),
    Y = factor(sample(c("n", "y"), 300, TRUE))
  )
  ## (1, b) never occurs, so both its arms end in the unobserved stage
  d <- d[!(d$X == "1" & d$TT == "b"), ]
  m <- join_unobserved(full(d, lambda = 0))
  s <- stages(m)[["Y"]]
  s[3:4] <- m$name_unobserved   # tie the arms of X = 1 in the unobserved stage
  stages(m)["Y"] <- s

  expect_equal(nrow(arm_separation(m, "TT", "Y")), 0)
  expect_equal(attr(arm_separation(m, "TT", "Y"), "n_ignored"), 1)
  expect_equal(nrow(arm_separation(m, "TT", "Y", ignore = NULL)), 1)
  ## and the unobserved stage is not split
  expect_identical(stages(separate_arms(m, "TT", "Y"))[["Y"]], s)
})

test_that("arm_separation and separate_arms check their arguments", {
  m <- full(Titanic, lambda = 1)
  expect_error(arm_separation(m, "Survived", "Age"), "follow")
  expect_error(separate_arms(m, "Survived", "Age"), "follow")
  expect_error(arm_separation(m, "NotAVar", "Survived"))
  expect_error(separate_arms(m, "NotAVar", "Survived"))
})

test_that("separate_arms needs the data, not only the probabilities", {
  ## the staging is replaced through stages<-, which refits; on an object
  ## with no ctables that branch erases the probabilities instead
  m <- random_sevt(list(X = c("a", "b"), TT = c("0", "1"), Y = c("n", "y")))
  s <- stages(m)[["Y"]]
  s[1:2] <- "tied"
  m$stages$Y <- s
  expect_error(separate_arms(m, "TT", "Y"), "ctables")
})

test_that("separate_arms never merges situations while refining", {
  ## the new name of a split situation must not land on a stage which is
  ## kept: ps_stratify mints names with the same ":" convention
  m <- full(Titanic, lambda = 1)
  s <- stages(m)[["Survived"]]
  s[1:2] <- "1"              # the tied arms of (1st, Male)
  s[c(5, 7)] <- "1:Child"    # a kept stage whose name the split would take
  stages(m)["Survived"] <- s

  ms <- separate_arms(m, "Age", "Survived")
  after <- stages(ms)[["Survived"]]
  expect_equal(nrow(arm_separation(ms, "Age", "Survived")), 0)
  expect_false(after[1] == after[5])
  expect_equal(after[5], after[7])   # the kept stage is untouched
  expect_gt(length(unique(after)), length(unique(s)))
})

test_that("separate_arms reports a split which leaves an arm with no data", {
  set.seed(3)
  d <- data.frame(
    X = factor(sample(c("0", "1"), 300, TRUE)),
    TT = factor(sample(c("a", "b"), 300, TRUE)),
    Y = factor(sample(c("n", "y"), 300, TRUE))
  )
  d <- d[!(d$X == "1" & d$TT == "b"), ]
  m <- join_unobserved(full(d, lambda = 0))
  s <- stages(m)[["Y"]]
  s[3:4] <- "pooled"
  stages(m)["Y"] <- s

  expect_warning(ms <- separate_arms(m, "TT", "Y"), "no observations")
  ## the empty half joins the unobserved stage rather than posing as a
  ## regular one with NA probabilities
  expect_true(m$name_unobserved %in% stages(ms)[["Y"]])
  regular <- setdiff(names(ms$prob$Y), m$name_unobserved)
  expect_false(any(vapply(regular, function(n) any(is.na(ms$prob$Y[[n]])), TRUE)))
})

test_that("arm_separation takes the same defaults as separate_arms", {
  m <- stages_bhc(full(Titanic, lambda = 1))
  expect_equal(
    as.data.frame(arm_separation(m)),
    as.data.frame(arm_separation(m, treatment = "Age", outcome = "Survived"))
  )
})

test_that("arm_separation counts the ties it hides, not the contexts", {
  set.seed(3)
  d <- data.frame(
    X = factor(sample(c("0", "1"), 300, TRUE)),
    TT = factor(sample(c("a", "b"), 300, TRUE)),
    Y = factor(sample(c("n", "y"), 300, TRUE))
  )
  d <- d[!(d$X == "1" & d$TT == "b"), ]
  m <- join_unobserved(full(d, lambda = 0))
  s <- stages(m)[["Y"]]
  s[3:4] <- m$name_unobserved
  stages(m)["Y"] <- s
  v <- arm_separation(m, "TT", "Y")
  expect_equal(attr(v, "n_ignored"), 1)
  ## and the print does not claim a separation it has not checked
  expect_output(print(v), "outside the ignored stages")
  expect_message(print(v), "not shown")
})
