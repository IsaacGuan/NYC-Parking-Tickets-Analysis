setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

uniformTimeAndYear <- function(violation_time_vehicle_year) {
	df <- violation_time_vehicle_year[FALSE,]
	time <- c()
	for (i in 1:nrow(violation_time_vehicle_year)) {
		hour <- as.numeric(substr(violation_time_vehicle_year[i,"Violation.Time"], 1, 2))
		minute <- as.numeric(substr(violation_time_vehicle_year[i,"Violation.Time"], 3, 4))
		clock <- substr(violation_time_vehicle_year[i,"Violation.Time"], 5, 5)
		if (violation_time_vehicle_year[i,"Vehicle.Year"] != 0) {
			if (clock == "A"){
				time[length(time)+1] <- hour*60 + minute
			} else if (clock == "P") {
				if (hour == 12) {
					time[length(time)+1] <- hour*60 + minute
				} else {
					time[length(time)+1] <- (hour+12)*60 + minute
				}
			}
			current_row = nrow(df) + 1
			df[current_row,"Vehicle.Year"] <- violation_time_vehicle_year[i,"Vehicle.Year"]
		}
	}
	df[["Violation.Time"]] <- time
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
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/correlation_violation_time_vehicle_year.jpg")
plot(violation_time, vehicle_year, main = "Correlation between Violation Time and Vehicle Year", xlab = "Violation Time", ylab = "Vehicle Year" , col="blue", pch = 4, xaxt = "n")
x_minutes <- axTicks(1)
x_times_reuniformed <- c()
for (i in 1:length(x_minutes)) {
	x_times_reuniformed[i] <- reuniformTime(x_minutes[i])
}
axis(1, at = x_minutes, labels = x_times_reuniformed)