# libraries



#=========================================================================#
# read and set project filters 
#=========================================================================#

file_path <- "/Users/arham/Downloads/02. MVS/Final Project/Dataset 1 — Gun violence.csv"
gun <- read.csv(file_path)
# filter to gun incidents ,i.e., n_guns_involved >1
gun <- subset(gun, n_guns_involved != 0)
# remove unnecessary columns
gun <- gun[, c("incident_id", "date", "state", "city_or_county", "n_killed", "n_injured", "congressional_district", "gun_stolen", "gun_type", "incident_characteristics",
               "n_guns_involved", "notes", "participant_age",
               "participant_age_group", "participant_gender", 
               "participant_relationship","participant_status", "participant_type")]

# gun <- na.omit(gun)

# remove columns
columns_to_remove <- c("incident_url_fields_missing", "location", "address", "sources", "incident_url", "source_url")
gun <- gun[, !(names(gun) %in% columns_to_remove)]
# remove state senate district and state house district
gun <- gun[, !(names(gun) %in% c("state_house_district", "state_senate_district"))]
# remove participant name
gun <- gun[, !(names(gun) %in% "participant_name")]
# remove participant age
gun <- gun[, !(names(gun) %in% "participant_age")]


###### Stolen Guns
# count number of occurences of stolen of gun_stolen and record in new column (a cell has values like :0::Unknown||1::Unknown )
gun$stolen_count <- sapply(gun$gun_stolen, function(x) length(strsplit(x, split = "\\|\\|")[[1]]))
# remove gun_stolen column
gun <- gun[, !(names(gun) %in% "gun_stolen")]



####### number of suspects, victims, and total people involved
# count number of Subject-Suspect, Victim, and Total people involved
gun$subject_count <- sapply(gun$participant_type, function(x) length(grep("Subject-Suspect", strsplit(x, split = "\\|\\|")[[1]])))
gun$victim_count <- sapply(gun$participant_type, function(x) length(grep("Victim", strsplit(x, split = "\\|\\|")[[1]])))
gun$total_count <- sapply(gun$participant_type, function(x) length(strsplit(x, split = "\\|\\|")[[1]]))
#remove participant_type column
gun <- gun[, !(names(gun) %in% "participant_type")]


####### number of injured, killed, unharmed arrested, and unharmed
# from participant_status column count number of Injured, Killed, and 'Unharmed, Arrested', and 'Unharmed'
gun$injured_count <- sapply(gun$participant_status, function(x) length(grep("Injured", strsplit(x, split = "\\|\\|")[[1]])))
gun$killed_count <- sapply(gun$participant_status, function(x) length(grep("Killed", strsplit(x, split = "\\|\\|")[[1]])))
gun$unharmed_arrested_count <- sapply(gun$participant_status, function(x) length(grep("Unharmed, Arrested", strsplit(x, split = "\\|\\|")[[1]])))
gun$unharmed_count <- sapply(gun$participant_status, function(x) length(grep("Unharmed", strsplit(x, split = "\\|\\|")[[1]])))


# remove participant_status column
gun <- gun[, !(names(gun) %in% "participant_status")]
# remove congressional district, state, city_or_county, latitude, longitude
gun <- gun[, !(names(gun) %in% c("congressional_district", "state", "city_or_county", "latitude", "longitude"))]

# from participant age group count number of Adult 18+ and Teen 12-17, child 0-11
gun$adult_count <- sapply(gun$participant_age_group, function(x) length(grep("Adult 18+", strsplit(x, split = "\\|\\|")[[1]])))
gun$teen_count <- sapply(gun$participant_age_group, function(x) length(grep("Teen 12-17", strsplit(x, split = "\\|\\|")[[1]])))
gun$child_count <- sapply(gun$participant_age_group, function(x) length(grep("Child 0-11", strsplit(x, split = "\\|\\|")[[1]])))
# remove participant_age_group column
gun <- gun[, !(names(gun) %in% "participant_age_group")]

# from participant_gender column calculate number of 'Male' and 'Female' occurences 
gun$male_count <- sapply(gun$participant_gender, function(x) length(grep("Male", strsplit(x, split = "\\|\\|")[[1]])))
gun$female_count <- sapply(gun$participant_gender, function(x) length(grep("Female", strsplit(x, split = "\\|\\|")[[1]])))
# remove participant_gender
gun <- gun[, !(names(gun) %in% "participant_gender")]


# select columns for clustering - don't select incident_id, date, notes, incident_characteristics, gun_type, participant_relationship
X <- gun[, !(names(gun) %in% c('female_count', 'adult_count', 'stolen_count', 'total_count', 'injured_count', 'killed_count',"incident_id", "date", "notes", "incident_characteristics", "gun_type", "participant_relationship", "location_description"))]



View(X)
# print first 5 rows of gun


# standarize X
OG <-X 
X <- scale(X)

# pca and clustering then ploting pca colorewd by cluster and with loadings marked
pca <- prcomp(X, scale = TRUE)
summary(pca)
plot(pca, type = "l")
biplot(pca, scale = 0)

################
library(purrr)
library(factoextra)

# Assuming 'pca' is your principal component analysis object
# Choose the number of clusters
optimal_k <- 8

# Perform clustering with the specified number of clusters
kmeans_pca <- kmeans(pca$x[, 1:2], centers = optimal_k, nstart = 25)
kmeans_pca$cluster <- as.factor(kmeans_pca$cluster)

# Create a scatter plot with cluster colors
plot(pca$x[, 1:2], col = kmeans_pca$cluster, pch = 20, cex = 2)

# Overlay cluster centers
points(kmeans_pca$centers, col = 1:optimal_k, pch = 3, cex = 3, lwd = 3)

# Add cluster labels to the plot
text(pca$x[, 1:2], labels = kmeans_pca$cluster, pos = 3)

# Biplot showing loadings
biplot(pca, scale = 0)

# Title and legend
title(main = "Cluster Colors and Loadings")
legend("topright", legend = levels(kmeans_pca$cluster), col = 1:optimal_k, pch = 20, title = "Clusters")

# Centres of clusters
kmeans_pca$centers

# Describe clusters without standardized values
cluster_descriptions <- aggregate(OG, by = list(kmeans_pca$cluster), FUN = mean)

# Print cluster descriptions
print(cluster_descriptions)

# Plot box plot by cluster for all variables
par(mfrow = c(3, 3))
for (i in 1:9) {
  boxplot(OG[, i] ~ kmeans_pca$cluster, main = names(OG)[i], xlab = "Cluster")
}


# clear previous plots
dev.off()
# Show cluster sizes
table(kmeans_pca$cluster)

library(ggplot2)

# Map cluster names to OG
OG$cluster <- as.factor(kmeans_pca$cluster)

# Create box plots for each variable by cluster using ggplot2
plots1 <- list()
for (i in 1:9) {
  p <- ggplot(OG, aes(x = cluster, y = OG[, i])) +
    geom_boxplot() +
    labs(title = names(OG)[i], x = "Cluster", y = names(OG)[i])
  
  plots1[[i]] <- p
}

View(OG)

# create OG  ggpairs colored by column 'cluster'
ggpairs(OG, columns = 1:9, aes(colour = cluster))
# view plot
View(plots1)

# using dataframe OG, i want a pivot table with columns as cluster and rows as variable names and values as mean of variable for that cluster


# OG column names
names(OG)

# Assuming your dataframe is named OG
library(dplyr)
library(stargazer)

# Create a vector of clusters (assuming clusters are integers from 1 to 8)
clusters <- 1:8

# Loop through each cluster
for (cluster_id in clusters) {
  # Filter the dataframe for the current cluster
  filtered_data <- OG %>% filter(cluster == cluster_id)
  
  # Print the stargazer table for the current cluster
  cat("Cluster", cluster_id, "Summary Statistics:\n")
  stargazer(filtered_data, type = "text")
  cat("\n")
}




library(ggplot2)
library(reshape2)

# Assuming clusters are integers from 1 to 8
clusters <- setdiff(1:8)  # Remove cluster 3

# Filter data for selected clusters
filtered_data <- OG[OG$cluster %in% clusters, ]

# Melt the data for plotting
melted_data <- melt(filtered_data, id.vars = c("cluster"))

# Create a box plot for each variable, colored by cluster, and faceted by variable
p <- ggplot(melted_data, aes(x = as.factor(cluster), y = value, fill = as.factor(cluster))) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(title = "Box Plots by Cluster",
       x = "Cluster",
       y = "Value") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8),      # Adjust strip text size
        axis.text.x = element_text(size = 8),     # Adjust x-axis text size
        axis.text.y = element_text(size = 8),     # Adjust y-axis text size
        axis.title = element_text(size = 10),     # Adjust axis title size
        strip.background = element_rect(fill = "white"))  # Set strip background color

# Adjust plot size for better visibility
options(repr.plot.width = 10, repr.plot.height = 8)

# Print the plot
print(p)


# 






