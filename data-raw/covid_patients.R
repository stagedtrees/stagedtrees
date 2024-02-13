library(stagedtrees)

data_model <- sevt(list(
  Sex = c("Female", "Male"),
  Age = c(
    "0-39", "40-49", "50-59", "60-69",
    "70-79", "80+"
  ),
  ICU = c("yes", "no"),
  death = c("yes", "no")
), full = TRUE)
data_model$prob <- list()
data_model$prob$Sex <- list("1" = c(Female = 0.45185, Male = 0.54815))

dist_age_male <- c(
  0.01616346, # 0 - 39
  0.04159445, # 40 - 49
  0.10130439, # 50 - 59
  0.16825686, # 60 - 69
  0.25217550, # 70 - 79
  0.42050534
) # 80+

dist_age_female <- c(
  0.01688613, # 0 - 39
  0.04271329, # 40 - 49
  0.10131681, # 50 - 59
  0.16841872, # 60 - 69
  0.25289366, # 70 - 79
  0.41777138
) # 80+

names(dist_age_male) <- data_model$tree$Age
names(dist_age_female) <- data_model$tree$Age
data_model$prob$Age <- list(
  "1" = dist_age_female,
  "2" = dist_age_male
)
data_model$prob$ICU <- list(
  "1" = c(yes = 0.125, no = 1 - 0.125), # Female 0-39
  "2" = c(yes = 0.149, no = 1 - 0.149), # Female 40-49
  "3" = c(yes = 0.193, no = 1 - 0.193), # Female 50-59
  "4" = c(yes = 0.225, no = 1 - 0.225), # Female 60-69
  "5" = c(yes = 0.175, no = 1 - 0.175), # Female 70-79
  "6" = c(yes = 0.037, no = 1 - 0.037), # Female 80+
  "7" = c(yes = 0.197, no = 1 - 0.197), # Male 0-39
  "8" = c(yes = 0.2687, no = 1 - 0.2687), # Male 40-49
  "9" = c(yes = 0.3171, no = 1 - 0.3171), # Male 50-59
  "10" = c(yes = 0.3415, no = 1 - 0.3415), # Male 60-69
  "11" = c(yes = 0.274, no = 1 - 0.274), # Male 70-79
  "12" = c(yes = 0.073, no = 1 - 0.073) # Male 80+
)
data_model$prob$death <- list(
  ################### FEMALE ################################
  "1"  = c(yes = 0.077, no = 1 - 0.077), # Female 0-39 ICU
  "2"  = c(yes = 0.004, no = 1 - 0.004), # Female 0-39 no-ICU
  "3"  = c(yes = 0.117, no = 1 - 0.117), # Female 40-49 ICU
  "4"  = c(yes = 0.017, no = 1 - 0.017), # Female 40-49 no-ICU
  "5"  = c(yes = 0.185, no = 1 - 0.185), # Female 50-59 ICU
  "6"  = c(yes = 0.030, no = 1 - 0.030), # Female 50-59 no-ICU
  "7"  = c(yes = 0.239, no = 1 - 0.239), # Female 60-69 ICU
  "8"  = c(yes = 0.058, no = 1 - 0.058), # Female 60-69 no-ICU
  "9"  = c(yes = 0.324, no = 1 - 0.324), # Female 70-79 ICU
  "10" = c(yes = 0.124, no = 1 - 0.124), # Female 70-79 no-ICU
  "11" = c(yes = 0.454, no = 1 - 0.454), # Female 80+ ICU
  "12" = c(yes = 0.266, no = 1 - 0.266), # Female 80+ no-ICU
  ################# MALE ##################################
  "13" = c(yes = 0.079, no = 1 - 0.079), # Male 0-39 ICU
  "14" = c(yes = 0.008, no = 1 - 0.008), # Male 0-39 no-ICU
  "15" = c(yes = 0.098, no = 1 - 0.098), # Male 40-49 ICU
  "16" = c(yes = 0.016, no = 1 - 0.016), # Male 40-49 no-ICU
  "17" = c(yes = 0.171, no = 1 - 0.171), # Male 50-59 ICU
  "18" = c(yes = 0.030, no = 1 - 0.030), # Male 50-59 no-ICU
  "19" = c(yes = 0.278, no = 1 - 0.278), # Male 60-69 ICU
  "20" = c(yes = 0.067, no = 1 - 0.067), # Male 60-69 no-ICU
  "21" = c(yes = 0.383, no = 1 - 0.383), # Male 70-79 ICU
  "22" = c(yes = 0.150, no = 1 - 0.150), # Male 70-79 no-ICU
  "23" = c(yes = 0.478, no = 1 - 0.478), # Male 80+ ICU
  "24" = c(yes = 0.363, no = 1 - 0.363) # Male 80+ no-ICU
)

covid_patients <- sample_from(data_model, 10000, seed = 123)
usethis::use_data(covid_patients, overwrite = TRUE)
