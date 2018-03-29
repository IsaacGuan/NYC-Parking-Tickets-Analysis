setwd("d:/Data-Mining-and-Concept-Learning/NYC-Parking-Tickets-Analysis/datasets")

n <- as.numeric(commandArgs(TRUE)[1])
costomer_profiling_data <- read.csv("Assign3_Customer_Profiling_nomalized.csv", head = TRUE, sep = ";", quote = "\"")
wss <- (nrow(costomer_profiling_data) - 1) * sum(apply(costomer_profiling_data, 2 , var))
for (i in 1:n) {
	for (j in 2:15) {
		wss[j] <- sum(kmeans(costomer_profiling_data, centers=j)$withinss)
	}
	#print(paste("d:/Data-Mining-and-Concept-Learning/Assignment-3/elbow_for_k_means_clustering_", i, ".jpg", sep = ""))
	jpeg(file = paste("d:/Data-Mining-and-Concept-Learning/Assignment-3/elbow_for_k_means_clustering_", i, ".jpg", sep = ""))
	plot(1:15, wss, type="b", main = paste("Elbow for K-Means Clustering Iteration", i), xlab="Number of Clusters", ylab="Within groups sum of squares")
}
