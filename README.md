[Link to Final Anonymous PDF](https://github.com/arham-anwar/Disecting_gun_violence_crime_scene_using_multivariate_stats/blob/main/Final_Anonymous.pdf)


# Unveiling Dichotomies in North American Gun Violence through Multivariate Insights

This project aims to delve into the multifaceted issue of gun violence in North America by leveraging advanced statistical techniques and machine learning algorithms. The analysis provides insights into patterns, characteristics, and relationships within a comprehensive dataset of gun-related incidents. This study utilizes K-Means Clustering and Principal Component Analysis (PCA) to identify distinct clusters and trends, offering a granular understanding of the incidents to inform preventive strategies.

## Table of Contents

1. [Introduction](#introduction)
2. [Data Description](#data-description)
3. [Feature Engineering](#feature-engineering)
4. [Model Selection & Methodology](#model-selection--methodology)
5. [Results](#results)
6. [Conclusion & Future Scope](#conclusion--future-scope)
7. [How to Run](#how-to-run)
8. [License](#license)

## Introduction

Gun violence is a critical societal issue, necessitating a comprehensive understanding of incidents to inform preventive strategies. This study employs a dataset of gun-related incidents to extract valuable insights. Through advanced statistical techniques and visualizations, this analysis aims to unravel patterns, characteristics, and relationships hidden behind recorded history and extend previous efforts in curating this dataset.

### Goals

- Identify patterns, trends, and clusters using feature engineering and clustering.
- Provide insights to guide targeted interventions.
- Contribute to informed decision-making for mitigating gun violence.

## Data Description

The dataset was sourced from Kaggle, titled “Gun Violence Data Comprehensive record of over 260k US gun violence incidents from 2013-2018”. It includes detailed information on each incident, such as the type of weapon used, number of victims, and relationships among participants.

### Feature Engineering

Key features engineered from the dataset include:
- **Lethality**: Weapon type and its lethality.
- **Participants**: Number of victims and suspects.
- **Kill-Death-Assist**: Number of injured, killed, unharmed arrested, and unharmed participants.
- **Age Profile**: Classification of participants by age group.
- **Gender Ratio**: Female percentage of participants.
- **Relationship Status**: Relationships between perpetrators and victims.

## Model Selection & Methodology

Three clustering algorithms were tested: DBScan, K-Median Clustering, Hierarchical clustering, and K-Means clustering. The final model selection was K-Means Clustering combined with PCA to reduce dimensionality.

### Decision of ‘K’

The optimal number of clusters was determined through iterative testing and analysis of the total weighted sum of squares and gap statistics. The best value of 'K' was found to be 9.

## Results

The application of PCA and K-Means clustering revealed distinct groupings within the gun violence dataset. The identified clusters provide a granular understanding of the incidents, ranging from smaller-scale urban conflicts to organized crime and extreme outlier events.

## Conclusion & Future Scope

This comprehensive analysis of gun violence incidents in the United States has successfully utilized advanced statistical techniques and clustering methodologies to extract meaningful insights. The findings advocate for targeted interventions and future extensions of the model to further inform preventive strategies.

## How to Run

### Prerequisites

- R (version 4.0 or later)
- RStudio (optional but recommended)
- Required R packages: `dplyr`, `factoextra`, `purrr`, `ggplot2`, `reshape2`, `stargazer`, `cluster`, `corrplot`

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/arhamanwar/gun-violence-analysis.git
   cd gun-violence-analysis
   ```

2. Install the required packages:
   ```r
   install.packages(c("dplyr", "factoextra", "purrr", "ggplot2", "reshape2", "stargazer", "cluster", "corrplot"))
   ```

3. Load the dataset and preprocess:
   ```r
   # Load necessary libraries
   library(dplyr)
   library(factoextra)
   library(purrr)
   library(ggplot2)
   library(reshape2)
   library(stargazer)
   library(cluster)
   library(corrplot)

   # Read and preprocess the dataset
   file_path <- "data/gun_violence.csv"
   gun <- read.csv(file_path)
   gun <- subset(gun, n_guns_involved != 0)
   gun <- na.omit(gun)
   
   # Feature engineering
   source("scripts/feature_engineering.R")

   # Save preprocessed data
   write.csv(gun, file = "data/gun_preprocessed.csv", row.names = FALSE)
   ```

4. Perform clustering analysis:
   ```r
   # Load preprocessed data
   gun <- read.csv("data/gun_preprocessed.csv")

   # Clustering and PCA
   source("scripts/clustering_analysis.R")
   ```

5. Generate visualizations and results:
   ```r
   # Generate visualizations
   source("scripts/visualizations.R")
   ```

### Running the Analysis

Run the following command in your R console to execute the entire analysis pipeline:
```r
source("scripts/run_analysis.R")
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
```
