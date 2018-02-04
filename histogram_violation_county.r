setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")
nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
violation_county <- nyc_data[["Violation.County"]]
violation_county_table <- table(violation_county)
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/bar_plot_violation_county.jpg")
barplot(violation_county_table, main = "Bar Plot of Violation County", xlab = "Violation County", ylab = "Violation Frequency")
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/histogram_violation_county.jpg")
hist(violation_county_table, main = "Histogram of Violation County", xlab = "Violation Frequency", ylab = "Violation County Number")