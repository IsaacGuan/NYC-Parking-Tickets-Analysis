setwd("d:/data mining and concept learning/Assignment 1/NYC-Parking-Tickets-Analysis/datasets")
nyc_data = read.csv("Parking_Violations_Issued_sampled.csv", head = TRUE, sep = ",", quote = "\"")
write.csv(nyc_data, "Parking_Violations_Issued_sampled_new.csv")