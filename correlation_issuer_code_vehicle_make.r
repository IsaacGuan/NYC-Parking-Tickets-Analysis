setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

normalize <- function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

uniformIssuerAndVehicle <- function (issuer_code_vehicle_make) {
	df <- issuer_code_vehicle_make[FALSE,]
	vehicle_make <- c()
	for (i in 1:nrow(issuer_code_vehicle_make)) {
		if (issuer_code_vehicle_make[i,"Issuer.Code"] != 0 && trim(issuer_code_vehicle_make[i,"Vehicle.Make"]) != "") {
			if (trim(issuer_code_vehicle_make[i,"Vehicle.Make"]) == "FR LI") {
				vehicle_make[length(vehicle_make)+1] <- "FR/LI"
			} else if (trim(issuer_code_vehicle_make[i,"Vehicle.Make"]) == "HIN") {
				vehicle_make[length(vehicle_make)+1] <- "HINO"
			} else if (trim(issuer_code_vehicle_make[i,"Vehicle.Make"]) == "UTIL") {
				vehicle_make[length(vehicle_make)+1] <- "UTILI"
			} else if (trim(issuer_code_vehicle_make[i,"Vehicle.Make"]) == "VAN H") {
				vehicle_make[length(vehicle_make)+1] <- "VANHO"
			} else {
				vehicle_make[length(vehicle_make)+1] <- trim(issuer_code_vehicle_make[i,"Vehicle.Make"])
			}
			current_row = nrow(df) + 1
			df[current_row, "Issuer.Code"] = issuer_code_vehicle_make[i,"Issuer.Code"]
		}
	}
	df[["Vehicle.Make"]] <- vehicle_make
	return (df)
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
issuer_code_vehicle_make <- subset(nyc_data, select = c("Issuer.Code", "Vehicle.Make"))
issuer_code_vehicle_make_uniformed <- uniformIssuerAndVehicle(issuer_code_vehicle_make)
issuer_code <- issuer_code_vehicle_make_uniformed[["Issuer.Code"]]
vehicle_make <- issuer_code_vehicle_make_uniformed[["Vehicle.Make"]]
issuer_code_factor <- as.integer(factor(issuer_code))
vehicle_make_factor <- as.integer(factor(vehicle_make))
issuer_code_vehicle_make_uniformed_factor <- data.frame(issuer_code_factor, vehicle_make_factor)
names(issuer_code_vehicle_make_uniformed_factor) <- c("Issuer.Code", "Vehicle.Make")
issuer_code_vehicle_make_uniformed_factor_normalized <- as.data.frame(lapply(issuer_code_vehicle_make_uniformed_factor[1:2], normalize))
#table(issuer_code_factor)
#table(vehicle_make_factor)
jpeg(file = "d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/diagrams/correlation_issuer_code_vehicle_make.jpg")
plot(issuer_code_factor, vehicle_make_factor, main = "Correlation between Issuer and Vehicle Make", xlab = "Issuer", ylab = "Vehicle Make", col="blue", pch = 4, xaxt = "n", yaxt = "n")
cor.test(issuer_code_factor, vehicle_make_factor, method = c("pearson"))
cor.test(issuer_code_factor, vehicle_make_factor, method = c("kendall"))
cor.test(issuer_code_factor, vehicle_make_factor, method = c("spearman"), exact = FALSE)
chisq.test(issuer_code_vehicle_make_uniformed_factor_normalized)