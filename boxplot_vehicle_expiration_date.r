setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

validateDate <- function(year, month, day) {
	if (year < 2013 || year > 2017) {
		return (FALSE)
	}
	if (month < 1 || month > 12) {
		return (FALSE)
	}
	if (day < 1 || day > 31) {
		return (FALSE)
	}
	return (TRUE)
}

uniformDate <- function(vehicle_expiration_date) {
	vr <- c()
	for (i in 1:length(vehicle_expiration_date)) {
		vehicle_expiration_date_string <- toString(vehicle_expiration_date[i])
		if (nchar(vehicle_expiration_date_string) == 8) {
			year = as.numeric(substr(vehicle_expiration_date_string, 1, 4))
			month = as.numeric(substr(vehicle_expiration_date_string, 5, 6))
			day = as.numeric(substr(vehicle_expiration_date_string, 7, 8))
			if (validateDate(year, month, day)) {
				days = year*365 + (month-1)*30 + day
				vr[length(vr)+1] <- days
			}
		}
		if (nchar(vehicle_expiration_date_string) == 26) {
			year = as.numeric(substr(vehicle_expiration_date_string, 7, 10))
			month = as.numeric(substr(vehicle_expiration_date_string, 11, 12))
			day = as.numeric(substr(vehicle_expiration_date_string, 13, 14))
			if (validateDate(year, month, day)) {
				days = as.numeric(year)*365 + (as.numeric(month)-1)*30 + as.numeric(day)
				vr[length(vr)+1] <- days
			}
		}
	}
	return (vr)
}

reuniformDate <- function(days) {
	year <- floor(days/365)
	month <- floor((days%%365)/30+1)
	if (month > 12) {
		month <- 12
		day <- 31
	} else {
		day <- round((days%%365)%%30)
	}
	return (paste(year, month, day, sep = "-"))
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
vehicle_expiration_date <- nyc_data[["Vehicle.Expiration.Date"]]
vehicle_expiration_date_uniformed <- uniformDate(vehicle_expiration_date)
vehicle_expiration_date_uniformed_sorted <- sort(vehicle_expiration_date_uniformed)
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/box_plot_vehicle_expiration_date.jpg")
boxplot(vehicle_expiration_date_uniformed_sorted, main = "Box Plot of Vehicle Expiration Date", yaxt = "n")
y_days <- axTicks(2)
y_dates_reuniformed <- c()
for (i in 1:length(y_days)) {
	y_dates_reuniformed[i] <- reuniformDate(y_days[i])
}
axis(2, at = y_days, labels = y_dates_reuniformed)
five_num_days <- boxplot.stats(vehicle_expiration_date_uniformed_sorted)$stats
five_num_dates_reuniformed <- c()
for (i in 1:length(five_num_days)) {
	five_num_dates_reuniformed[i] <- reuniformDate(five_num_days[i])
}
text(y = boxplot.stats(vehicle_expiration_date_uniformed_sorted)$stats, labels = five_num_dates_reuniformed, x = 1.25)