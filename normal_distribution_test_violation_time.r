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
violation_time <- nyc_data[["Violation.Time"]]
violation_time_uniformed <- uniformTime(violation_time)
#violation_time_uniformed_sorted <- sort(violation_time_uniformed)
#violation_time_uniformed_sorted
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/density_plot_violation_time.jpg")
plot(density(violation_time_uniformed), main = "Density Plot of Violation Time", xaxt = "n")
x_minutes <- axTicks(1)
x_times_reuniformed <- c()
for (i in 1:length(x_minutes)) {
	x_times_reuniformed[i] <- reuniformTime(x_minutes[i])
}
axis(1, at = x_minutes, labels = x_times_reuniformed)
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/qq_plot_violation_time.jpg")
qqnorm(violation_time_uniformed, main = "Q-Q Plot of Violation Time", yaxt = "n")
qqline(violation_time_uniformed)
y_minutes <- axTicks(2)
y_times_reuniformed <- c()
for (i in 1:length(y_minutes)) {
	y_times_reuniformed[i] <- reuniformTime(y_minutes[i])
}
axis(2, at = y_minutes, labels = y_times_reuniformed)
shapiro.test(violation_time_uniformed)