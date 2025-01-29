# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(igraph)
library(ggplot2)

# Load the dataset
data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\PTS 5 (exploring gender bias in PTS)\\pca_transformed_cleveland_gender.csv", row.names = NULL)
head(data)

# Extract relevant columns
features <- data[, 1:2]  # PCA components (first two columns)
age <- data$Age          # Age column
target <- data$Class     # Disease severity column
gender <- data$Sex       # Gender column (0 = female, 1 = male)

# Split the dataset by gender
male_data <- data %>% filter(Sex == 1)
female_data <- data %>% filter(Sex == 0)

# Function to generate PTS for a specific dataset
generate_pts <- function(features, age, target, nreps = 50, sampsize = 30, endclass = 4) {
  sampsize <- min(sampsize, nrow(features))  # Ensure sample size is within dataset size
  dis <- as.matrix(dist(features, method = "euclidean"))  # Distance matrix
  cons <- dis  # Initialize constraint matrix
  
  # Apply constraints to penalize backward transitions in age
  for (i in 1:nrow(features)) {
    for (j in 1:nrow(features)) {
      if (age[j] < age[i]) {
        cons[i, j] <- 999  # Penalize backward age transitions
      }
    }
  }
  
  pts <- list()  # Store PTS trajectories
  for (i in 1:nreps) {
    repeat {
      # Sample indices ensuring the presence of both start (youngest) and end (oldest) classes
      mysamp <- sample(1:nrow(features), sampsize, replace = TRUE)
      if (is.element(0, target[mysamp]) & is.element(endclass, target[mysamp])) {
        break
      }
    }
    
    dunesamp <- features[mysamp, ]  # Sampled features
    dissamp <- cons[mysamp, mysamp]  # Constrained distance matrix
    
    # Build minimum spanning tree (MST)
    mode(dissamp) <- "numeric"
    g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
    datamst <- mst(g)  # Minimum spanning tree
    
    # Identify start (youngest) and end (oldest) points
    startp <- which(age[mysamp] == min(age[mysamp]))[1]
    endp <- which(age[mysamp] == max(age[mysamp]))[1]
    
    # Get shortest path in the MST from start to end
    datshort <- shortest_paths(datamst, startp, endp, mode = "all", output = "both")
    
    # Extract trajectory indices
    ptsind <- datshort$vpath[[1]]
    pts[[i]] <- mysamp[ptsind]
  }
  
  return(pts)
}

# Generate PTS for males and females
male_pts <- generate_pts(male_data[, 1:2], male_data$Age, male_data$Class)
female_pts <- generate_pts(female_data[, 1:2], female_data$Age, female_data$Class)

# PCA for visualization
male_pca <- princomp(male_data[, 1:2])
female_pca <- princomp(female_data[, 1:2])

### Scatterplots for Male Patients
# Scatterplot of all male patients colored by disease stage
ggplot(male_data, aes(x = X, y = Y, color = factor(Class))) +
  geom_point( size =3) +
  labs(
    title = "Scatterplot of Male Patients (Colored by Stage)",
    x = "PCA Component 1", y = "PCA Component 2", color = "Class"
  ) +
  theme_minimal()

# Scatterplot with trajectories for male patients
ggplot() +
  geom_point(data = male_data, aes(x = X, y = Y, color = factor(Class)), alpha = 0.6, size =3) +
  lapply(male_pts, function(pt) {
    geom_path(data = male_data[pt, ], aes(x = X, y = Y), color = "black", alpha = 0.4, size = 0.11)
  }) +
  labs(
    title = "PTS Trajectories for Male Patients",
    x = "PCA Component 1", y = "PCA Component 2", color = "Class"
  ) +
  theme_minimal()

### Scatterplots for Female Patients
# Scatterplot of all female patients colored by disease stage
ggplot(female_data, aes(x = X, y = Y, color = factor(Class))) +
  geom_point(size = 3) +
  labs(
    title = "Scatterplot of Female Patients (Colored by Stage)",
    x = "PCA Component 1", y = "PCA Component 2", color = "Class"
  ) +
  theme_minimal()

# Scatterplot with trajectories for female patients
ggplot() +
  geom_point(data = female_data, aes(x = X, y = Y, color = factor(Class)), alpha = 0.6, size = 3) +
  lapply(female_pts, function(pt) {
    geom_path(data = female_data[pt, ], aes(x = X, y = Y), color = "black", alpha = 0.4, size = 0.1)
  }) +
  labs(
    title = "PTS Trajectories for Female Patients",
    x = "PCA Component 1", y = "PCA Component 2", color = "Class"
  ) +
  theme_minimal()

