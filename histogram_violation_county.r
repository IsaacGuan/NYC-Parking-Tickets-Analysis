setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

uniformCounty <- function(violation_county) {
	vr <- c()
	for (i in 1:length(violation_county)) {
		if (trim(violation_county[i]) != "") {
			vr[length(vr)+1] <- trim(violation_county[i])
		}
	}
	return (vr)
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
violation_county <- nyc_data[["Violation.County"]]
violation_county_uniformed <- uniformCounty(violation_county)
violation_county_uniformed_table <- table(violation_county_uniformed)
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/bar_plot_violation_county.jpg")
barplot(violation_county_uniformed_table, main = "Bar Plot of Violation County", xlab = "Violation County", ylab = "Violation Frequency")
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/histogram_violation_county.jpg")
hist(violation_county_uniformed_table, main = "Histogram of Violation County", xlab = "Violation Frequency", ylab = "Violation County Number")