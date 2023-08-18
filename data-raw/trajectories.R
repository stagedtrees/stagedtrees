library("stagedtrees")

tree <- list(SEX = c("male", "female"),
             AGE = c("child", "adult", "elder"),
             ICU = c("0", "1"),
             RSP = c("intub", "mask", "no"),
             OUT = c("death", "survived"))

model <- sevt(tree, full = TRUE)

stages(model)["ICU", AGE = "child"] <- "ICUchild"

stages(model)["ICU", SEX = "male", AGE = "elder"] <-
  stages(model)["ICU", SEX = "female", AGE = "elder"]

stages(model)["RSP", AGE = c("child"), ICU = "0"] <- "childnoICU"
stages(model)["RSP", AGE = c("child"), ICU = "1"] <- "childICU"

stages(model)["RSP", AGE = c("adult")] <- stages(model)["RSP", AGE = c("elder")]


stages(model)["OUT", AGE = "adult",
              SEX = "female",
              ICU = "1",
              RSP = c("intub", "mask")] <- "femaleICUresp"

stages(model)["OUT", AGE = "child",
                     ICU = "1",
                     RSP = "intub"] <- "childICUintub"

stages(model)["OUT", AGE = "child",
              ICU = "1",
              RSP = "mask"] <- "childICUmask"

stages(model)["OUT", AGE = "child",
              ICU = "1",
              RSP = "no"] <- "childICUno"

stages(model)["OUT", AGE = "adult", SEX = "male"] <-
  stages(model)["OUT", AGE = "elder", SEX = "female"]

stages(model)["OUT", ICU = "0", RSP = "intub"] <- "UNOBS"
stages(model)["OUT", ICU = "0", RSP = "intub"] <- "UNOBS"
stages(model)["OUT", AGE = "child", ICU = "0"] <- "UNOBS"

model$prob <- list()
model$prob$SEX <- list( "NA" = c(male = 0.4, female = 0.6))
model$prob$AGE <- list("1" = c("child" = 0.1, "adult" = 0.5, "elder" = 0.4),
                       "2" = c("child" = 0.1, "adult" = 0.3, "elder" = 0.6))

model$prob$ICU <- list("ICUchild" = c("0" = 0, "1" = 1),
                       "2" = c("0" = 0.4, "1" = 0.6), ## male adult
                       "5" = c("0" = 0.2, "1" = 0.8), ## female adult
                       "6" = c("0" = 0.7, "1" = 0.3)) ## elder

model$prob$RSP <- list("childnoICU" = c("intub" = NA, "mask" = NA, "no" = NA),
                       "childICU" = c("intub" = 0.1, "mask" = 0.7, "no" = 0.2),
                       "5" = c("intub" = 0, "mask" = 0.7, "no" = 0.3), # male noICU
                       "6" = c("intub" = 0.4, "mask" = 0.5, "no" = 0.1), # male ICU
                       "11" = c("intub" = 0, "mask" = 0.5, "no" = 0.5), # female noICU
                       "12" = c("intub" = 0.4, "mask" = 0.5, "no" = 0.1)) # female ICU


model$prob$OUT <- list("UNOBS" = c("death" = NA, "survived" = NA),
                       "childICUintub" = c("death" = 0.03, "survived" = 0.97),
                       "childICUmask" = c("death" = 0.02, "survived" = 0.98),
                       "childICUno" = c("death" = 0.01, "survived" = 0.99),
                       ### male adult and female elder ICU = 0 :
                       "32" = c("death" = 0.05, "survived" = 0.95), ## mask
                       "33" = c("death" = 0.01, "survived" = 0.99), ## no
                       ### male adult and female elder ICU = 1 :
                       "34" = c("death" = 0.15, "survived" = 0.85), ## intub
                       "35" = c("death" = 0.08, "survived" = 0.92), ## mask
                       "36" = c("death" = 0.04, "survived" = 0.96), ## no
                       ##############
                       "14" = c("death" = 0.2, "survived" = 0.8), # male elder 0 mask
                       "15" = c("death" = 0.1, "survived" = 0.9), # male elder 0 no
                       "16" = c("death" = 0.3, "survived" = 0.7), # male elder 1 intub
                       "17" = c("death" = 0.25, "survived" = 0.75), # male elder 1 mask
                       "18" = c("death" = 0.3, "survived" = 0.7), # male elder 1 no
                       ##############
                       "26" = c("death" = 0.1, "survived" = 0.9), # female adult 0 mask
                       "27" = c("death" = 0.15, "survived" = 0.85), # female adult 0 no
                       "30" = c("death" = 0.2, "survived" = 0.8),  # female adult 1 no
                       ##############
                       "femaleICUresp" = c("death" = 0.1, "survived" = 0.9)
                       )


trajectories <- sample_from(model, 10000, seed = 1)