setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

uniformTimeAndYear <- function(violation_time_vehicle_year) {
	violation_time <- c()
	vehicle_year <- c()
	for (i in 1:nrow(violation_time_vehicle_year)) {
		hour <- as.numeric(substr(violation_time_vehicle_year[i,"Violation.Time"], 1, 2))
		minute <- as.numeric(substr(violation_time_vehicle_year[i,"Violation.Time"], 3, 4))
		clock <- substr(violation_time_vehicle_year[i,"Violation.Time"], 5, 5)
		if (violation_time_vehicle_year[i,"Vehicle.Year"] != 0) {
			if (clock == "A"){
				violation_time[length(violation_time)+1] <- hour*60 + minute
			} else if (clock == "P") {
				if (hour == 12) {
					violation_time[length(violation_time)+1] <- hour*60 + minute
				} else {
					violation_time[length(violation_time)+1] <- (hour+12)*60 + minute
				}
			}
			vehicle_year[length(vehicle_year)+1] <- violation_time_vehicle_year[i,"Vehicle.Year"]
		}
	}
	df <- data.frame(violation_time, vehicle_year)
	names(df) <- c("Violation.Time", "Vehicle.Year")
	return (df)
}

reuniformTime <- function(time_uniformed) {
	hour <- round(time_uniformed/60)
	minute <- round(time_uniformed%%60)
	clock <- ""
	if (hour > 23) {
		hour <- hour - 24
	}
	if (hour < 12) {
		clock <- "AM"
	} else {
		if (hour == 12) {
			clock <- "PM"
		} else {
			hour <- hour - 12
			clock <- "PM"
		}
	}
	if (hour < 10) {
		hour <- paste("0", hour, sep = "")
	}
	if (minute < 10) {
		minute <- paste("0", minute, sep = "")
	}
	return (paste(hour, ":", minute, " ", clock, sep = ""))
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
violation_time_vehicle_year <- subset(nyc_data, select = c("Violation.Time", "Vehicle.Year"))
violation_time_vehicle_year_uniformed <- uniformTimeAndYear(violation_time_vehicle_year)
violation_time_vehicle_year_uniformed_sorted <- violation_time_vehicle_year_uniformed[order(violation_time_vehicle_year_uniformed[,1]), ]
violation_time <- violation_time_vehicle_year_uniformed_sorted[["Violation.Time"]]
vehicle_year <- violation_time_vehicle_year_uniformed_sorted[["Vehicle.Year"]]
violation_time_vehicle_year_uniformed_sorted_normalized <- as.data.frame(lapply(violation_time_vehicle_year_uniformed_sorted[1:2], normalize))
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/correlation_violation_time_vehicle_year.jpg")
plot(violation_time_vehicle_year_uniformed_sorted, main = "Correlation between Violation Time and Vehicle Year", xlab = "Violation Time", ylab = "Vehicle Year" , col="blue", pch = 4, xaxt = "n")
x_minutes <- axTicks(1)
x_times_reuniformed <- c()
for (i in 1:length(x_minutes)) {
	x_times_reuniformed[i] <- reuniformTime(x_minutes[i])
}
axis(1, at = x_minutes, labels = x_times_reuniformed)
cor.test(violation_time, vehicle_year, method = c("pearson"))
cor.test(violation_time, vehicle_year, method = c("kendall"))
cor.test(violation_time, vehicle_year, method = c("spearman"), exact = FALSE)
chisq.test(violation_time_vehicle_year_uniformed_sorted_normalized)