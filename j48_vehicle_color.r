setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")
library("RWeka")

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new_filtered.csv", head = TRUE, sep = ",", quote = "\"")
nyc_data_cleaned <- nyc_data
nyc_data_cleaned_matrix <- as.matrix(nyc_data_cleaned)
nyc_data_cleaned_matrix[is.na(nyc_data_cleaned_matrix)] <- ""
nyc_data_cleaned <- data.frame(nyc_data_cleaned_matrix)
nyc_data_cleaned_factored <- lapply(nyc_data_cleaned, as.factor)
target <- nyc_data_cleaned_factored[["Vehicle.Color"]]
plate_type <- nyc_data_cleaned_factored[["Plate.Type"]]
vehicle_body_type <- nyc_data_cleaned_factored[["Vehicle.Body.Type"]]
vehicle_make <- nyc_data_cleaned_factored[["Vehicle.Make"]]
vehicle_year <- nyc_data_cleaned_factored[["Vehicle.Year"]]
registration_state <- nyc_data_cleaned_factored[["Registration.State"]]
result_J48 <- J48(target ~ plate_type + vehicle_body_type + vehicle_make + vehicle_year + registration_state, nyc_data_cleaned_factored)
summary(result_J48)