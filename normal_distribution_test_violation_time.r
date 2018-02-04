setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

uniformTime <- function(violation_time) {
	vr <- c()
	for (i in 1:length(violation_time)) {
		hour <- as.numeric(substr(violation_time[i], 1, 2))
		minute <- as.numeric(substr(violation_time[i], 3, 4))
		clock <- substr(violation_time[i], 5, 5)
		time <- 0
		if (clock == "A"){
			vr[i] <- hour*60 + minute
		} else if (clock == "P") {
			if (hour == 12) {
				vr[i] <- hour*60 + minute
			} else {
				vr[i] <- (hour+12)*60 + minute
			}
		}
	}
	return (vr)
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
violation_time <- nyc_data[["Violation.Time"]]
violation_time_uniformed <- uniformTime(violation_time)
#violation_time_uniformed_sorted <- sort(violation_time_uniformed)
#violation_time_uniformed_sorted
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/density_plot_violation_time.jpg")
plot(density(violation_time_uniformed), main = "Density Plot of Violation Time")
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/qq_plot_violation_time.jpg")
qqnorm(violation_time_uniformed, main = "Q-Q Plot of Violation Time")
qqline(violation_time_uniformed)
shapiro.test(violation_time_uniformed)