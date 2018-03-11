setwd("d:/data mining and concept learning/NYC-Parking-Tickets-Analysis/datasets")
library("RWeka")

nyc_data_major <- read.arff("Assignment2_Agency_relabeled_major_undersampled.arff")
nyc_data_minor <- read.arff("Assignment2_Agency_relabeled_minor_oversampled.arff")
nyc_data_balanced <- rbind(nyc_data_major, nyc_data_minor)
write.arff(nyc_data_balanced, "Assignment2_Agency_relabeled_balanced.arff")
