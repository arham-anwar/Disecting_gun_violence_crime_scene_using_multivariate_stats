
#=========================================================================#
# libraries 
#=========================================================================#

# libary 
#install.packages("cluster")
#install.packages("factoextra") 

# Load necessary libraries
library(dplyr)
library(factoextra)
library(purrr)
library(ggplot2)
library(reshape2)
library(stargazer)
library(cluster)
library(factoextra)
library(cluster)



#=========================================================================#
# read and set project filters 
#=========================================================================#

file_path <- "/Users/arham/Downloads/02. MVS/Final Project/Dataset 1 — Gun violence.csv"
gun <- read.csv(file_path)

gun <- subset(gun, n_guns_involved != 0) # filter to gun incidents ,i.e., n_guns_involved >1
# filter to relevant columns
gun <- gun[, c("incident_id", "date", "state", "city_or_county", "latitude", "longitude", "n_killed", "n_injured", "congressional_district", "gun_stolen", "gun_type", "incident_characteristics",
               "n_guns_involved", "notes", "participant_age",
               "participant_age_group", "participant_gender", 
               "participant_relationship","participant_status", "participant_type")]

#### Prelimary filter to columns which are not repeated or non-redundant
gun <- na.omit(gun)

## "state", "city_or_county", "latitude", "longitude"

#=========================================================================#
# read and set project filters 
#=========================================================================#

###### Stolen Guns
# count number of occurences of stolen of gun_stolen and record in new column (a cell has values like :0::Unknown||1::Unknown )
gun$stolen_count <- sapply(gun$gun_stolen, function(x) length(strsplit(x, split = "\\|\\|")[[1]]))
# remove gun_stolen column
gun <- gun[, !(names(gun) %in% "gun_stolen")]

###### gun_types

# First, convert all the text in the "gun_type" column to lowercase
gun$gun_type <- tolower(gun$gun_type)
# Count the number of occurrences of specific keywords in the "gun_type" column, split using "||"
gun$n_handguns <- sapply(gun$gun_type, function(x) length(grep("handgun", strsplit(x, split = "\\|\\|")[[1]])))
gun$n_auto <- sapply(gun$gun_type, function(x) length(grep("auto", strsplit(x, split = "\\|\\|")[[1]])))
gun$n_mm <- sapply(gun$gun_type, function(x) length(grep("mm", strsplit(x, split = "\\|\\|")[[1]])))
gun$n_spl <- sapply(gun$gun_type, function(x) length(grep("spl", strsplit(x, split = "\\|\\|")[[1]])))
gun$n_mag <- sapply(gun$gun_type, function(x) length(grep("mag", strsplit(x, split = "\\|\\|")[[1]])))
# Sum up counts and remove unnecessary columns
gun$n_handguns <- gun$n_handguns + gun$n_auto + gun$n_mm + gun$n_spl + gun$n_mag 
gun <- gun[, !(names(gun) %in% c("n_auto", "n_mm", "n_spl", "n_mag", "n_win"))]
# Count occurrences of 'win' and 'rifle', adjust counts, sum them up, and remove unnecessary columns
gun$n_win <- sapply(gun$gun_type, function(x) length(grep("win", strsplit(x, split = "\\|\\|")[[1]])))

gun$n_rifle <- sapply(gun$gun_type, function(x) length(grep("rifle", strsplit(x, split = "\\|\\|")[[1]])))
gun$n_rifle <- gun$n_rifle + gun$n_win
gun <- gun[, !(names(gun) %in% c("n_win"))]
# Count occurrences of 'gauge' and 'shotgun', adjust counts, sum them up, and remove unnecessary columns
gun$n_gauge <- sapply(gun$gun_type, function(x) length(grep("gauge", strsplit(x, split = "\\|\\|")[[1]])))
gun$n_shotgun <- sapply(gun$gun_type, function(x) length(grep("shotgun", strsplit(x, split = "\\|\\|")[[1]])))
gun$n_shotgun <- gun$n_shotgun + gun$n_gauge
gun <- gun[, !(names(gun) %in% c("n_gauge"))]
# Count the number of occurrences of "||", add 1 to it, and subtract counts of shotguns, rifles, and handguns
gun$n_other <- sapply(gun$gun_type, function(x) sum(gregexpr("\\|\\|", x)[[1]] > 0))
gun$n_other <- gun$n_other - gun$n_shotgun - gun$n_rifle - gun$n_handguns + 1
# remove gun_type column
gun <- gun[, !(names(gun) %in% "gun_type")]



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


# from participant age group count number of Adult 18+ and Teen 12-17, child 0-11
gun$adult_count <- sapply(gun$participant_age_group, function(x) length(grep("Adult 18+", strsplit(x, split = "\\|\\|")[[1]])))
gun$teen_count <- sapply(gun$participant_age_group, function(x) length(grep("Teen 12-17", strsplit(x, split = "\\|\\|")[[1]])))
gun$child_count <- sapply(gun$participant_age_group, function(x) length(grep("Child 0-11", strsplit(x, split = "\\|\\|")[[1]])))
# remove participant_age_group column
gun <- gun[, !(names(gun) %in% "participant_age_group")]


## gender ratio

calculate_female_percentage <- function(participant_gender) {
  genders <- strsplit(participant_gender, "\\|\\|")[[1]]
  total_participants <- length(genders)
  female_count <- sum(grepl("Female", genders))
  
  if (total_participants > 0) {
    return((female_count / total_participants) * 100)
  } else {
    return(NA)
  }
}

# Apply the function to create a new column for female percentage
gun <- gun %>%
  mutate(female_percentage = sapply(participant_gender, calculate_female_percentage))
# remove participant_status column
gun <- gun[, !(names(gun) %in% "participant_status")]


# save as gun_preprocessed_v1.csv
write.csv(gun, file = "gun_preprocessed_v1.csv", row.names = FALSE)
# read gun_preprocessed_v1.csv
gun <- read.csv("gun_preprocessed_v1.csv")


tempo = gun 
# remove congressional district, state, city_or_county, latitude, longitude
gun <- gun[, !(names(gun) %in% c("participant_age","congressional_district", "state", "city_or_county", "latitude", "longitude"))]
# Columns to exclude from clustering
exclude_columns <- c('incident_id', 'date', 'notes', 'incident_characteristics', 'gun_type', 'participant_relationship', 'location_description',
                     'participant_gender', 'adult_count', 'gun_stolen','unharmed_count','stolen_count', 'victim_count', 'total_count', 'injured_count', 'killed_count','n_guns_involved')
X <- gun[, !(names(gun) %in% exclude_columns)]

#rename n_other to n_stolen_unkown
names(X)[names(X) == 'n_other'] <- 'n_stolen_unknown'

# X[is.infinite(X)] <- 0
X[is.na(X)] <- 0
class(X)
#  X as dataframe
X <- as.data.frame(X)
OG <-X 
X <- scale(X)
X <- as.data.frame(X)
W <- X
Z <- X

View(X)
# beautiful correlation table visualization
library(corrplot)

# correlation matrix

corr <- cor(X)

corrplot(corr, method = "shade")



# ========================================================================#
## K Median Clustering
# ========================================================================#

# W 
View(W)


##### What should be the k? 
set.seed(123)  # for reproducibility
sample_indices <- sample(nrow(W), 1000)  # adjust the size as needed
subset_W <- W[sample_indices, ]
# make the graph square in ratio 1:1
par(pty = "m")
fviz_nbclust(subset_W[, -1], pam, method = "wss")


#calculate gap statistic based on number of clusters
gap_stat <- clusGap(subset_W[, -1],
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

# k = 7


# k means clustering
set.seed(123)
kmeans_W <- kmeans(W[, -1], centers = 7, nstart = 25)
kmeans_W$cluster <- as.factor(kmeans_W$cluster)

# add cluster column to W
W$cluster <- kmeans_W$cluster

pca <- prcomp(W[, -c(1, ncol(W))], scale = TRUE)

library(ggplot2)
library(ggfortify)

names(W)

# autoplot(pca, data = W[, -1], colour = 'cluster', loadings = TRUE, loadings.label = TRUE) +
#   theme_minimal() +
#   theme(legend.position = 'bottom') +
#   ggtitle("PCA Colored by Cluster")

autoplot(pca, data = W[, -1], loadings = TRUE, loadings.label = TRUE) +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  ggtitle("Crime Scene Incident Split")

# autoplot clusters
autoplot(pca, data = W[, -1], colour = 'cluster', loadings = TRUE, loadings.label = TRUE,loadings.color = 'black', loadings.label.color = 'black') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  ggtitle("PCA Colored by Cluster")

# add cluster names to OG by incident id
OG$cluster_kmeans <- W$cluster

OG



# # Results_kmeans <- OG %>%
# #   group_by(cluster_kmeans) %>%
# #   summarise(across(everything(), list(
# #     count = ~n(),
# #     mean = ~mean(.),
# #     sd = ~sd(.),
# #     median = ~median(.),
# #     min = ~min(.),
# #     max = ~max(.)
# #   ), .names = "{.col}_{.fn}"))
# View(Results_kmeans)

# for each variable create a histogram by clusters
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(stargazer)

# Create box plots for each variable by cluster using ggplot2
plots <- lapply(1:9, function(i) {
  ggplot(OG, aes(x = cluster_kmeans, y = OG[, i], fill = cluster_kmeans)) +
    geom_boxplot() +
    labs(title = names(OG)[i], x = "Cluster", y = names(OG)[i])
})

# Print the box plots
print(plots)




#=========================================================================#
# Load required libraries
library(purrr)
library(factoextra)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(stargazer)

# Function to perform PCA and clustering
perform_pca_clustering <- function(X, optimal_k = 8) {
  # PCA
  pca <- prcomp(X, scale = TRUE)
  
  # K-means clustering
  kmeans_pca <- kmeans(pca$x[, 1:2], centers = optimal_k, nstart = 25)
  kmeans_pca$cluster <- as.factor(kmeans_pca$cluster)
  
  # Plot PCA colored by cluster
  plot(pca$x[, 1:2], col = kmeans_pca$cluster, pch = 20, cex = 2)
  points(kmeans_pca$centers, col = 1:optimal_k, pch = 3, cex = 3, lwd = 3)
  text(pca$x[, 1:2], labels = kmeans_pca$cluster, pos = 3)
  
  # Biplot showing loadings
  biplot(pca, scale = 0)
  title(main = "Cluster Colors and Loadings")
  legend("topright", legend = levels(kmeans_pca$cluster), col = 1:optimal_k, pch = 20, title = "Clusters")
  
  # Cluster descriptions
  cluster_descriptions <- aggregate(X, by = list(kmeans_pca$cluster), FUN = mean)
  print(cluster_descriptions)
  
  # Create box plots for each variable by cluster using ggplot2
  OG <- as.data.frame(X)
  OG$cluster <- as.factor(kmeans_pca$cluster)
  
  plots <- lapply(1:9, function(i) {
    ggplot(OG, aes(x = cluster, y = OG[, i], fill = cluster)) +
      geom_boxplot() +
      labs(title = names(OG)[i], x = "Cluster", y = names(OG)[i])
  })
  
  # Print the box plots
  print(plots)
  
  # Create a pivot table with columns as cluster and rows as variable names
  pivot_table <- OG %>%
    group_by(cluster) %>%
    summarise(across(everything(), mean)) %>%
    pivot_wider(names_from = cluster, values_from = everything())
  
  # Print the pivot table
  print(pivot_table)
  
  # Return the result
  return(OG)
}



# Use the perform_pca_clustering function
result <- perform_pca_clustering(X, optimal_k = 8)

# Generate stargazer tables for each cluster
clusters <- 1:8
for (cluster_id in clusters) {
  filtered_data <- result %>% filter(cluster == cluster_id)
  cat("Cluster", cluster_id, "Summary Statistics:\n")
  stargazer(filtered_data, type = "text")
  cat("\n")
}

# Create box plots for selected clusters using ggplot2
selected_clusters <- setdiff(1:8, 3)
filtered_data <- result[result$cluster %in% selected_clusters, ]
melted_data <- melt(filtered_data, id.vars = c("cluster"))
p <- ggplot(melted_data, aes(x = as.factor(cluster), y = value, fill = as.factor(cluster))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(title = "Box Plots by Cluster", x = "Cluster", y = "Value") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        strip.background = element_rect(fill = "white"))

# Print the plot
options(repr.plot.width = 10, repr.plot.height = 8)
print(p)


#------------------------------------------------------------------------


#### data ingest
exclude_columns2 <- c('incident_id', 'date', 'notes', 'incident_characteristics', 'gun_type', 'participant_relationship', 'location_description',
                     'participant_gender', 'adult_count', 'gun_stolen','unharmed_count','stolen_count', 'victim_count', 'total_count', 'injured_count', 'killed_count','n_guns_involved')
D <- gun[, !(names(gun) %in% exclude_columns2)]

#rename n_other to n_stolen_unkown
names(W)[names(W) == 'n_other'] <- 'n_stolen_unknown'
D <- gun[,!(names(gun) %in% exclude_columns2)]

D[is.na(W)] <- 0

# standarize all W except first column, which is going to be used as index
D[,2:ncol(W)] <- scale(W[,2:ncol(W)])

## dbscan clustering
library(factoextra)
library(cluster)

# Compute DBSCAN using the Euclidean distance matrix
db <- dbscan(D[, -1], eps = 0.5)

# Plot DBSCAN results
fviz_cluster(db, data = D[, -1], geom = "point", stand = FALSE, frame = FALSE, 
             ggtheme = theme_minimal(), palette = "jco", 
             show.clust.cent = TRUE, show.clust.borders = TRUE, 
             pointsize = 2, repel = TRUE, labelsize = 10, 
             main = "DBSCAN Clustering Results")

# add cluster column to D
D$cluster <- db$cluster

pca <- prcomp(D[, -c(1, ncol(D))], scale = TRUE)

library(ggplot2)
library(ggfortify)

names(D)

# autoplot(pca, data = D[, -1], colour = 'cluster', loadings = TRUE, loadings.label = TRUE) +
#   theme_minimal() +
#   theme(legend.position = 'bottom') +
#   ggtitle("PCA Colored by Cluster")

autoplot(pca, data = D[, -1], loadings = TRUE, loadings.label = TRUE) +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  ggtitle("Crime Scene Incident Split")

# autoplot clusters
