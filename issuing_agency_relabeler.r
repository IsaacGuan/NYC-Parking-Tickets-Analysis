setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")
library("RWeka")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

issuingagencyRelabeler <- function (nyc_data) {
	issuing_agency <- c()
	nyc_data_relabeled <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (trim(nyc_data[i, "Issuing Agency"]) != "T"
			&& trim(nyc_data[i, "Issuing Agency"]) != "P"
			&& trim(nyc_data[i, "Issuing Agency"]) != "X"
			&& trim(nyc_data[i, "Issuing Agency"]) != "V"
			&& trim(nyc_data[i, "Issuing Agency"]) != "S") {
			issuing_agency[length(issuing_agency)+1] <- "Other"
		} else {
			issuing_agency[length(issuing_agency)+1] <- trim(nyc_data[i, "Issuing Agency"])
		}
	}
	nyc_data_relabeled[["Issuing Agency"]] <- issuing_agency
	return (nyc_data_relabeled)
}

nyc_data <- read.arff("Assignment2_Agency.arff")
nyc_data_issuing_agency_relabeled <- issuingagencyRelabeler(nyc_data)
write.arff(nyc_data_issuing_agency_relabeled, "Assignment2_Agency_relabeled.arff")
