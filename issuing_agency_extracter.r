setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")
library("RWeka")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

majorExtracter <- function(nyc_data) {
	data_to_drop <- c()
	nyc_data_extracted <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (trim(nyc_data[i, "Issuing Agency"]) != "T"
			&& trim(nyc_data[i, "Issuing Agency"]) != "P") {
			data_to_drop[length(data_to_drop)+1] <- i
		}
	}
	if (length(data_to_drop) > 0) {
		nyc_data_extracted <- nyc_data[-data_to_drop,]
	}
	return (nyc_data_extracted)
}

minorExtracter <- function(nyc_data) {
	data_to_drop <- c()
	nyc_data_extracted <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (trim(nyc_data[i, "Issuing Agency"]) == "T"
			|| trim(nyc_data[i, "Issuing Agency"]) == "P") {
			data_to_drop[length(data_to_drop)+1] <- i
		}
	}
	if (length(data_to_drop) > 0) {
		nyc_data_extracted <- nyc_data[-data_to_drop,]
	}
	return (nyc_data_extracted)
}

nyc_data <- read.arff("Assignment2_Agency_relabeled.arff")
nyc_data_major_extracted <- majorExtracter(nyc_data)
nyc_data_minor_extracted <- minorExtracter(nyc_data)
#table(nyc_data_major_extracted[["Issuing Agency"]])
#table(nyc_data_minor_extracted[["Issuing Agency"]])
write.arff(nyc_data_major_extracted, "Assignment2_Agency_relabeled_major.arff")
write.arff(nyc_data_minor_extracted, "Assignment2_Agency_relabeled_minor.arff")
