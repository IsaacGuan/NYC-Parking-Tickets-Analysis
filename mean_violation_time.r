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

reuniformTime <- function(time_uniformed) {
	hour <- round(time_uniformed/60)
	minute <- round(time_uniformed%%60)
	clock <- ""
	if (hour < 12) {
		clock <- "AM"
	} else {
		clock <- "PM"
	}
	if (hour < 10) {
		hour <- paste("0", hour, sep = "")
	}
	if (minute < 10) {
		minute <- paste("0", minute, sep = "")
	}
	return (paste(hour, minute, clock))
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
violation_time <- nyc_data[["Violation.Time"]]
violation_time_uniformed <- uniformTime(violation_time)
violation_time_uniformed_mean <- mean(violation_time_uniformed)
violation_time_mean <- reuniformTime(violation_time_uniformed_mean)
cat("the mean value of the violation time:", violation_time_mean)