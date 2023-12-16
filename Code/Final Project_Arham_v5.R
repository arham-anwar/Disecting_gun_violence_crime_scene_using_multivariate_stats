
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
library(dplyr)
library(stargazer)
library(ggplot2)
library(ggfortify)



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

#=========================================================================#
# read and set project filters 
#=========================================================================#

##============================ 1. Stolen Guns


# count number of occurences of stolen of gun_stolen and record in new column (a cell has values like :0::Unknown||1::Unknown )
gun$stolen_count <- sapply(gun$gun_stolen, function(x) length(strsplit(x, split = "\\|\\|")[[1]]))
# remove gun_stolen column
gun <- gun[, !(names(gun) %in% "gun_stolen")]

##============================ 2. gun_types



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


##============================ 3.  number of suspects, victims, and total people involved
# count number of Subject-Suspect, Victim, and Total people involved
gun$subject_count <- sapply(gun$participant_type, function(x) length(grep("Subject-Suspect", strsplit(x, split = "\\|\\|")[[1]])))
gun$victim_count <- sapply(gun$participant_type, function(x) length(grep("Victim", strsplit(x, split = "\\|\\|")[[1]])))
gun$total_count <- sapply(gun$participant_type, function(x) length(strsplit(x, split = "\\|\\|")[[1]]))
#remove participant_type column
gun <- gun[, !(names(gun) %in% "participant_type")]


##============================ 4. number of injured, killed, unharmed arrested, and unharmed
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


##============================ 5. gender ratio

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

##============================ 6. relations between participants
gun$participant_relationship <- tolower(gun$participant_relationship)
# Family
# Random victims
# Aquaintance
# Significant Others
# Armed Robbery
# Gang
# Mass Shooting
# Knows victims
# Co-worker
# Neighbor
# Friends
# Home Invasion
# Does Not Know Victim

# New column relation_family =  if family is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_family = ifelse(grepl("family", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_random_victims =  if random victims is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_random_victims = ifelse(grepl("random victims", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_aquaintance =  if aquaintance is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_aquaintance = ifelse(grepl("aquaintance", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_significant_others =  if significant others is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_significant_others = ifelse(grepl("significant others", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_armed_robbery =  if armed robbery is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_armed_robbery = ifelse(grepl("armed robbery", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_gang =  if gang is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_gang = ifelse(grepl("gang", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_mass_shooting =  if mass shooting is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_mass_shooting = ifelse(grepl("mass shooting", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_knows_victims =  if knows victims is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_knows_victims = ifelse(grepl("knows victims", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_co_worker =  if co-worker is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_co_worker = ifelse(grepl("co-worker", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_neighbor =  if neighbor is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_neighbor = ifelse(grepl("neighbor", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_friends =  if friends is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_friends = ifelse(grepl("friends", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_home_invasion =  if home invasion is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_home_invasion = ifelse(grepl("home invasion", participant_relationship, ignore.case = TRUE), 1, 0))
# New column relation_does_not_know_victim =  if does not know victim is present in participant_relationship column, then 1 else 0
gun <- gun %>% mutate(relation_does_not_know_victim = ifelse(grepl("does not know victim", participant_relationship, ignore.case = TRUE), 1, 0))
# remove participant_relationship column
gun <- gun[, !(names(gun) %in% "participant_relationship")]










#=========================================================================#
# More Feature Engineering
#=========================================================================#

# Apply the function to create a new column for female percentage
gun <- gun %>%
  mutate(female_percentage = sapply(participant_gender, calculate_female_percentage))
# remove participant_status column
gun <- gun[, !(names(gun) %in% "participant_status")]


# save as gun_preprocessed_v1.csv
write.csv(gun, file = "gun_preprocessed_vF.csv", row.names = FALSE)
#gun <- read.csv("gun_preprocessed_vF.csv")

tempo = gun 

selected_columns <- c(
  "child_count",
  "relation_family",
  "relation_random_victims",
  "relation_aquaintance",
  "relation_significant_others",
  "relation_armed_robbery",
  "relation_gang",
  "relation_mass_shooting",
  "relation_knows_victims",
  "relation_co_worker",
  "relation_neighbor",
  "relation_friends",
  "relation_home_invasion",
  "relation_does_not_know_victim"
)

gun_pca_result <- prcomp(gun[, selected_columns])
principal_components <- as.data.frame(gun_pca_result$x[, 1:2])
names(principal_components) <- c("relation_PC1", "relation_PC2")
gun <- cbind(gun[, -which(names(gun) %in% selected_columns)], principal_components)
gun <- gun[, !(names(gun) %in% selected_columns)]

gun <- gun[, !(names(gun) %in% c("participant_age","congressional_district", "state", "city_or_county", "latitude", "longitude"))]
exclude_columns <- c('incident_id', 'date', 'notes', 'incident_characteristics', 'gun_type', 'participant_relationship', 'location_description',
                     'participant_gender', 'adult_count', 'gun_stolen','unharmed_count','stolen_count', 'victim_count', 'total_count', 'injured_count', 'killed_count','n_guns_involved')
X <- gun[, !(names(gun) %in% exclude_columns)]


names(gun)

#rename n_other to n_stolen_unkown
names(X)[names(X) == 'n_other'] <- 'n_stolen_unknown'

# X[is.infinite(X)] <- 0
X[is.na(X)] <- 0
OG <-X 
X <- scale(X)
X <- as.data.frame(X)
W <- X
Z <- X


######### correlation
library(corrplot)
corr <- cor(X)
corrplot(corr, method = "shade")



# ========================================================================#
## K Means Clustering
# ========================================================================#

W <- X

#================= What should be the k? 

## support 1
set.seed(123)  # for reproducibility
sample_indices <- sample(nrow(W), 1000)  # adjust the size as needed
subset_W <- W[sample_indices, ]
par(pty = "m")
fviz_nbclust(subset_W, pam, method = "wss")

## support 2
#calculate gap statistic based on number of clusters
##gap_stat <- clusGap(subset_W,
                    #FUN = pam,
                    #K.max = 10, #max clusters to consider
                    #B = 50) #total bootstrapped iterations

###plot number of clusters vs. gap statistic
##viz_gap_stat(gap_stat)

# k = 9


#================= apply k means for 9 clusters


set.seed(123)
kmeans_W <- kmeans(W, centers = 9, nstart = 25)
kmeans_W$cluster <- as.factor(kmeans_W$cluster)
# add cluster column to W
W$cluster <- as.factor(kmeans_W$cluster)
OG$cluster <- as.factor(kmeans_W$cluster)

summary_by_cluster <- OG %>%
  group_by(cluster) %>%
  summarise(
    size = n(),
    mean_n_killed = round(mean(n_killed),2),
    mean_n_injured = round(mean(n_injured),2),
    mean_n_handguns = round(mean(n_handguns),2),
    mean_n_rifle = round(mean(n_rifle),2),
    mean_n_shotgun = round(mean(n_shotgun),2),
    mean_n_stolen_unknown = round(mean(n_stolen_unknown),2),
    mean_subject_count = round(mean(subject_count),2),
    mean_unharmed_arrested_count = round(mean(unharmed_arrested_count),2),
    mean_teen_count = round(mean(teen_count),2),
    mean_female_percentage = round(mean(female_percentage),2),
    mean_relation_PC1 = round(mean(as.numeric(relation_PC1)),2),
    mean_relation_PC2 = round(mean(as.numeric(relation_PC2)),2)
  )



summary_by_cluster = data.frame(summary_by_cluster)
summary_by_cluster = t(summary_by_cluster)
colnames(summary_by_cluster) <- summary_by_cluster[1, ]
summary_by_cluster <- summary_by_cluster[-1, ]


stargazer(summary_by_cluster, title = "Cluster Profiles",type = "html", digits = 2)


# pca on W excluding cluster column
pca <- prcomp(W[, -ncol(W)], scale = TRUE)
# autoplot(pca, data = W[, -1], colour = 'cluster', loadings = TRUE, loadings.label = TRUE) +
#   theme_minimal() +
#   theme(legend.position = 'bottom') +
#   ggtitle("PCA Colored by Cluster")


# autoplot clusters
autoplot(pca, data = W[, -1], colour = 'cluster', loadings = TRUE,loadings.label = TRUE,loadings.color = 'grey', loadings.label.color = 'navyblue') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  ggtitle("Visual Representation of Clusters with respect to eigen vectors")

# # autoplot clusters
# autoplot(pca, data = W[, -1], colour = 'cluster', loadings = TRUE, loadings.label = TRUE,loadings.color = 'black', loadings.label.color = 'black') +
#   theme_minimal() +
#   theme(legend.position = 'bottom') +
#   ggtitle("PCA Colored by Cluster")
# 
# #autoplot(pca, data = W[, -1], loadings = TRUE, loadings.label = TRUE) +
#   theme_minimal() +
#   theme(legend.position = 'bottom') +
#   ggtitle("Crime Scene Incident Split")


# add cluster names to OG by incident id
tempo$cluster_kmeans <- W$cluster

guns_f = tempo 

centroid_col_list <- c(
  "n_killed",
  "n_injured",
  "n_guns_involved",
  "stolen_count",
  "n_handguns",
  "n_rifle",
  "n_shotgun",
  "n_other",
  "subject_count",
  "victim_count",
  "total_count",
  "injured_count",
  "killed_count",
  "unharmed_arrested_count",
  "unharmed_count",
  "adult_count",
  "teen_count",
  "child_count",
  "relation_family",
  "relation_random_victims",
  "relation_aquaintance",
  "relation_significant_others",
  "relation_armed_robbery",
  "relation_gang",
  "relation_mass_shooting",
  "relation_knows_victims",
  "relation_co_worker",
  "relation_neighbor",
  "relation_friends",
  "relation_home_invasion",
  "relation_does_not_know_victim",
  "female_percentage",
  "cluster_kmeans")

# filter guns_f to only columns in centroid_col_list 
guns_f1 <- guns_f[, centroid_col_list]

guns_f1[is.na(guns_f1)] <- 0

# convert all columns to numeric
guns_f1 <- sapply(guns_f1, as.numeric)

guns_f1 = data.frame(guns_f1)

summary_by_cluster_f1 <- guns_f1 %>% group_by(cluster_kmeans) %>% summarise(
  size = n(),
  mean_n_killed = round(mean(n_killed), 2),
  mean_n_injured = round(mean(n_injured), 2),
  mean_n_guns_involved = round(mean(n_guns_involved), 2),
  mean_stolen_count = round(mean(stolen_count), 2),
  mean_n_handguns = round(mean(n_handguns), 2),
  mean_n_rifle = round(mean(n_rifle), 2),
  mean_n_shotgun = round(mean(n_shotgun), 2),
  mean_n_other = round(mean(n_other), 2),
  mean_subject_count = round(mean(subject_count), 2),
  mean_victim_count = round(mean(victim_count), 2),
  mean_total_count = round(mean(total_count), 2),
  mean_injured_count = round(mean(injured_count), 2),
  mean_killed_count = round(mean(killed_count), 2),
  mean_unharmed_arrested_count = round(mean(unharmed_arrested_count), 2),
  mean_unharmed_count = round(mean(unharmed_count), 2),
  mean_adult_count = round(mean(adult_count), 2),
  mean_teen_count = round(mean(teen_count), 2),
  mean_child_count = round(mean(child_count), 2),
  mean_relation_family = round(mean(relation_family), 2),
  mean_relation_random_victims = round(mean(relation_random_victims), 2),
  mean_relation_aquaintance = round(mean(relation_aquaintance), 2),
  mean_relation_significant_others = round(mean(relation_significant_others), 2),
  mean_relation_armed_robbery = round(mean(relation_armed_robbery), 2),
  mean_relation_gang = round(mean(relation_gang), 2),
  mean_relation_mass_shooting = round(mean(relation_mass_shooting), 2),
  mean_relation_knows_victims = round(mean(relation_knows_victims), 2),
  mean_relation_co_worker = round(mean(relation_co_worker), 2),
  mean_relation_neighbor = round(mean(relation_neighbor), 2),
  mean_relation_friends = round(mean(relation_friends), 2),
  mean_relation_home_invasion = round(mean(relation_home_invasion), 2)
)
summary_by_cluster_f1 = t(summary_by_cluster_f1)

colnames(summary_by_cluster_f1) <- summary_by_cluster_f1[1, ]
summary_by_cluster_f1 <- summary_by_cluster_f1[-1, ]
summary_by_cluster_f1 <- summary_by_cluster_f1[-nrow(summary_by_cluster_f1), ]
summary_by_cluster_f1 <- round(summary_by_cluster_f1, 2)

# stargazer summary_by_cluster_f1
stargazer(summary_by_cluster_f1, title = "Cluster Profiles",type = "text",column.sep.width = "5pt", digits = 2)


# save tempo to csv
#write.csv(tempo, file = "Final_Clustering.csv", row.names = FALSE)

# # read tempo
# tempo <- read.csv("Final_Clustering.csv")

guns_clusters = tempo
# replace blank with 0
guns_clusters[is.na(guns_clusters)] <- 0



##-----------------------------------------------------------------------
## Section 5 - Appendix and the lens of an analyst
##-----------------------------------------------------------------------



## 1 cluster Centroids
cols  = list(names(guns_clusters))

View(cols)

centroid_col_list <- c(
  "n_killed",
  "n_injured",
  "n_guns_involved",
  "stolen_count",
  "n_handguns",
  "n_rifle",
  "n_shotgun",
  "n_other",
  "subject_count",
  "victim_count",
  "total_count",
  "injured_count",
  "killed_count",
  "unharmed_arrested_count",
  "unharmed_count",
  "adult_count",
  "teen_count",
  "child_count",
  "relation_family",
  "relation_random_victims",
  "relation_aquaintance",
  "relation_significant_others",
  "relation_armed_robbery",
  "relation_gang",
  "relation_mass_shooting",
  "relation_knows_victims",
  "relation_co_worker",
  "relation_neighbor",
  "relation_friends",
  "relation_home_invasion",
  "relation_does_not_know_victim",
  "female_percentage",
  "cluster_kmeans")


guns_clusters_subset = guns_clusters[, centroid_col_list]

View(guns_clusters_subset)
# show column data types
sapply(guns_clusters_subset, class)
# convert all columns to numeric
guns_clusters_subset <- sapply(guns_clusters_subset, as.numeric)
guns_clusters_subset = data.frame(guns_clusters_subset)
#create median table with cluster_kmeans in columns, and all rest variables in rows
median_table = aggregate(guns_clusters_subset, list(guns_clusters_subset$cluster_kmeans), mean)

#Transpose table(median_table)
median_table = t(median_table)
View(median_table)


# use cluster_kmeans as column names
colnames(median_table) <- median_table[1,]
# remove first row
median_table <- median_table[-1,]
# remove last row
median_table <- median_table[-nrow(median_table),]
# remove killed_count row
median_table <- median_table[-which(rownames(median_table) == "killed_count"),]
# Round the median_table to 2 decimal places
rounded_median_table <- round(median_table, 2)



###########

library(tidyr)
library(tidyverse)

# convert date to date format
guns_f$date <- as.Date(guns_f$date)
# create a new column year-month column
guns_f$year_month <- format(guns_f$date, "%Y-%m")


# Create a table of the number of incidents per cluster per year-month
incidents_per_cluster_per_year_month <- guns_f %>%
  group_by(cluster_kmeans, year_month) %>%
  summarise(n = n())

print(incidents_per_cluster_per_year_month)

library(tidyverse)

df_pivoted <- incidents_per_cluster_per_year_month %>%
  pivot_wider(names_from = cluster_kmeans, values_from = n)

# If you want to fill missing values with 0
df_pivoted[is.na(df_pivoted)] <- 0

# Print the pivoted data frame
print(df_pivoted)

df_pivoted = data.frame(df_pivoted)

