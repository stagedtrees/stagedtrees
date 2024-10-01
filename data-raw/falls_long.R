## readxl and reshape2 files required

download.file("https://github.com/g-walley/cegpy/raw/refs/heads/main/data/Falls_Data.xlsx", "Falls_Data.xlsx")
data <- readxl::read_xlsx("Falls_Data.xlsx")
data$Treatment[is.na(data$Treatment)] <- "Not Referred & Not Treated"
res1 <- reshape2::colsplit(data$HousingAssessment, " ", names = c("Housing", "Assessment"))
res2 <- reshape2::colsplit(data$Treatment, " & ", names = c("Referred", "Treatment"))

falls_long <- cbind(res1, data[, "Risk"], res2, data[, "Fall"])

file.remove("Falls_Data.xlsx")
usethis::use_data(falls_long, overwrite = TRUE)
