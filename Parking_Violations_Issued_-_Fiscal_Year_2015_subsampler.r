nyc_data = read.csv("d:/data mining and concept learning/nyc-parking-tickets/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", head = TRUE, sep = ",", quote = "\"")
nyc_sampled <- nyc_data[sample(1:nrow(nyc_data), 1200, replace = FALSE),]
write.csv(nyc_sampled, "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets/Parking_Violations_Issued_-_Fiscal_Year_2015_sampled.csv")