setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

vehiclecolorFilter <- function (nyc_data) {
	vehicle_color <- c()
	data_to_drop <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (trim(nyc_data[i, "Vehicle.Color"]) == "") {
			data_to_drop[length(data_to_drop)+1] <- i
		} else {
			if (trim(nyc_data[i, "Vehicle.Color"]) == "BGE"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BEIGE") {
				vehicle_color[length(vehicle_color)+1] <- "BEIGE"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "BLACK"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BK"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BLK"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BLK."
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WHBK") {
				vehicle_color[length(vehicle_color)+1] <- "BLACK"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "PURPLE"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "PURPL"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BKPR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BUR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "PR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "LAVEN") {
				vehicle_color[length(vehicle_color)+1] <- "PURPLE"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "BLUE"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BL"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BLU"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "DKB"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "DKBLU"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "LTB"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "LTBL"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GYBL") {
				vehicle_color[length(vehicle_color)+1] <- "BLUE"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "BROWN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BRN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BRON"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BRW"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BRWN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "TAWNY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "TAN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "TN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "RDT") {
				vehicle_color[length(vehicle_color)+1] <- "BROWN"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "GOLD"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GL") {
				vehicle_color[length(vehicle_color)+1] <- "GOLD"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "GREEN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GRN"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GYGR") {
				vehicle_color[length(vehicle_color)+1] <- "GREEN"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "GREY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GRAY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GRY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "GYGY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BKG"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "DKG"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "DKGY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BKG"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "LTG"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "LTGY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WHGY"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BLG") {
				vehicle_color[length(vehicle_color)+1] <- "GREY"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "ORANGE"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "OR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "ORANG") {
				vehicle_color[length(vehicle_color)+1] <- "ORANGE"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "RED"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "RD"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WH/RD"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "MR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "MAR") {
				vehicle_color[length(vehicle_color)+1] <- "RED"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "SILVER"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "SIL"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "SILVE"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "SILVR"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "SL") {
				vehicle_color[length(vehicle_color)+1] <- "SILVER"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "WHITE"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WH"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WH/"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WHI"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WHO"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WHT"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "WT"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BLW"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "BLWH"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "RDW"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "RD/WT") {
				vehicle_color[length(vehicle_color)+1] <- "WHITE"
			} else if (trim(nyc_data[i, "Vehicle.Color"]) == "YELLOW"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "YELL"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "YELLO"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "YL"
				|| trim(nyc_data[i, "Vehicle.Color"]) == "YW") {
				vehicle_color[length(vehicle_color)+1] <- "YELLOW"
			} else {
				data_to_drop[length(data_to_drop)+1] <- i
			}
		}
	}
	if (length(data_to_drop) > 0) {
		nyc_data_filtered <- nyc_data[-data_to_drop,]
	}
	nyc_data_filtered <- nyc_data[-data_to_drop,]
	nyc_data_filtered[["Vehicle.Color"]] <- vehicle_color
	return (nyc_data_filtered)
}


platetypeFilter <- function (nyc_data) {
	plate_type <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (trim(nyc_data[i, "Plate.Type"]) == "999") {
			plate_type[length(plate_type)+1] <- "PAS"
		} else {
			plate_type[length(plate_type)+1] <- trim(nyc_data[i, "Plate.Type"])
		}
	}
	nyc_data_filtered[["Plate.Type"]] <- plate_type
	return (nyc_data_filtered)
}

vehiclebodytypeFilter <- function (nyc_data) {
	vehicle_body_type <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (trim(nyc_data[i, "Vehicle.Body.Type"]) == "2DSD"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "2 DR"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "2D") {
			vehicle_body_type[length(vehicle_body_type)+1] <- "2DSD"
		} else if (trim(nyc_data[i, "Vehicle.Body.Type"]) == "4DSD"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "4 DO"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "4 DR"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "4D"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "4D S"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "4H"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "4S") {
			vehicle_body_type[length(vehicle_body_type)+1] <- "4DSD"
		} else if (trim(nyc_data[i, "Vehicle.Body.Type"]) == "MOTO"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "MOT") {
			vehicle_body_type[length(vehicle_body_type)+1] <- "MOTO"
		} else if (trim(nyc_data[i, "Vehicle.Body.Type"]) == "MCY"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "MC") {
			vehicle_body_type[length(vehicle_body_type)+1] <- "MCY"
		} else if (trim(nyc_data[i, "Vehicle.Body.Type"]) == "PICK"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "PK") {
			vehicle_body_type[length(vehicle_body_type)+1] <- "PICK"
		} else if (trim(nyc_data[i, "Vehicle.Body.Type"]) == "SUBN"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "SU") {
			vehicle_body_type[length(vehicle_body_type)+1] <- "SUBN"
		} else if (trim(nyc_data[i, "Vehicle.Body.Type"]) == "UTIL"
			|| trim(nyc_data[i, "Vehicle.Body.Type"]) == "UT") {
			vehicle_body_type[length(vehicle_body_type)+1] <- "UTIL"
		} else {
			vehicle_body_type[length(vehicle_body_type)+1] <- trim(nyc_data[i, "Vehicle.Body.Type"])
		}
	}
	nyc_data_filtered[["Vehicle.Body.Type"]] <- vehicle_body_type
	return (nyc_data_filtered)
}

vehiclemakeFilter <- function (nyc_data) {
	vehicle_make <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (trim(nyc_data[i, "Vehicle.Make"]) == "FR LI") {
			vehicle_make[length(vehicle_make)+1] <- "FR/LI"
		} else if (trim(nyc_data[i, "Vehicle.Make"]) == "HIN") {
			vehicle_make[length(vehicle_make)+1] <- "HINO"
		} else if (trim(nyc_data[i, "Vehicle.Make"]) == "UTIL") {
			vehicle_make[length(vehicle_make)+1] <- "UTILI"
		} else if (trim(nyc_data[i, "Vehicle.Make"]) == "VAN H") {
			vehicle_make[length(vehicle_make)+1] <- "VANHO"
		} else {
			vehicle_make[length(vehicle_make)+1] <- trim(nyc_data[i, "Vehicle.Make"])
		}
	}
	nyc_data_filtered[["Vehicle.Make"]] <- vehicle_make
	return (nyc_data_filtered)
}

vehicleyearFilter <- function (nyc_data) {
	vehicle_year <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (nyc_data[i, "Vehicle.Year"] == 0) {
			vehicle_year[length(vehicle_year)+1] <- ""
		} else {
			vehicle_year[length(vehicle_year)+1] <- nyc_data[i, "Vehicle.Year"]
		}
	}
	nyc_data_filtered[["Vehicle.Year"]] <- vehicle_year
	return (nyc_data_filtered)
}

issuercodeFilter <- function (nyc_data) {
	issuer_code <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (nyc_data[i, "Issuer.Code"] == 0) {
			issuer_code[length(issuer_code)+1] <- ""
		} else {
			issuer_code[length(issuer_code)+1] <- nyc_data[i, "Issuer.Code"]
		}
	}
	nyc_data_filtered[["Issuer.Code"]] <- issuer_code
	return (nyc_data_filtered)
}

violationprecinctFilter <- function (nyc_data) {
	violation_precinct <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (nyc_data[i, "Violation.Precinct"] == 0) {
			violation_precinct[length(violation_precinct)+1] <- ""
		} else {
			violation_precinct[length(violation_precinct)+1] <- nyc_data[i, "Violation.Precinct"]
		}
	}
	nyc_data_filtered[["Violation.Precinct"]] <- violation_precinct
	return (nyc_data_filtered)
}

registrationstateFilter <- function (nyc_data) {
	registration_state <- c()
	nyc_data_filtered <- nyc_data
	for (i in 1:nrow(nyc_data)) {
		if (nyc_data[i, "Registration.State"] == 99) {
			registration_state[length(registration_state)+1] <- "NY"
		} else {
			registration_state[length(registration_state)+1] <- trim(nyc_data[i, "Registration.State"])
		}
	}
	nyc_data_filtered[["Registration.State"]] <- registration_state
	return (nyc_data_filtered)
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
nyc_data_filtered <- vehiclecolorFilter(nyc_data)
nyc_data_filtered <- platetypeFilter(nyc_data_filtered)
nyc_data_filtered <- vehiclebodytypeFilter(nyc_data_filtered)
nyc_data_filtered <- vehiclemakeFilter(nyc_data_filtered)
nyc_data_filtered <- vehicleyearFilter(nyc_data_filtered)
nyc_data_filtered <- issuercodeFilter(nyc_data_filtered)
nyc_data_filtered <- violationprecinctFilter(nyc_data_filtered)
nyc_data_filtered <- registrationstateFilter(nyc_data_filtered)
#table(nyc_data_filtered[["Vehicle.Color"]])
#table(nyc_data_filtered[["Plate.Type"]])
#table(nyc_data_filtered[["Vehicle.Body.Type"]])
#table(nyc_data_filtered[["Vehicle.Make"]])
#table(nyc_data_filtered[["Vehicle.Year"]])
#table(nyc_data_filtered[["Issuer.Code"]])
#table(nyc_data_filtered[["Violation.Precinct"]])
write.csv(nyc_data_filtered, "Parking_Violations_Issued_sampled_new_filtered.csv")
