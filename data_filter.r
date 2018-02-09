setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

colorFilter <- function (nyc_data) {
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
	nyc_data_filtered <- nyc_data[-data_to_drop,]
	nyc_data_filtered[["Vehicle.Color"]] <- vehicle_color
	return (nyc_data_filtered)
}

nyc_data <- read.csv("Parking_Violations_Issued_sampled_new.csv", head = TRUE, sep = ",", quote = "\"")
nyc_data_filtered <- colorFilter(nyc_data)
#table(nyc_data_filtered[["Vehicle.Color"]])
write.csv(nyc_data_filtered, "Parking_Violations_Issued_sampled_new_filtered.csv")
