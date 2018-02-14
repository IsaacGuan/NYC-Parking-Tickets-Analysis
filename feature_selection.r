setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")
library("RWeka")

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new_filtered.csv", head = TRUE, sep = ",", quote = "\"")
nyc_data_cleaned <- nyc_data
nyc_data_cleaned_matrix <- as.matrix(nyc_data_cleaned)
nyc_data_cleaned_matrix[is.na(nyc_data_cleaned_matrix)] <- ""
nyc_data_cleaned <- data.frame(nyc_data_cleaned_matrix)
nyc_data_cleaned_factored <- lapply(nyc_data_cleaned, as.factor)
target <- nyc_data_cleaned_factored[["Vehicle.Color"]]
result_gainratio <- GainRatioAttributeEval(target~., nyc_data_cleaned_factored)
summary(result_gainratio)