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

# Function to generate PTS with both Age and Disease Stage constraints
generate_pts_age_stage <- function(features, age, stage, nreps = 50, sampsize = 30, endclass = 4) {
  sampsize <- min(sampsize, nrow(features))  # Ensure sample size is within dataset size
  dis <- as.matrix(dist(features, method = "euclidean"))  # Distance matrix
  cons <- dis  # Initialize constraint matrix
  
  # Apply constraints to penalize backward transitions in Age and Disease Stage
  for (i in 1:nrow(features)) {
    for (j in 1:nrow(features)) {
      if (stage[j] < stage[i] && age[j] < age[i]) {
        cons[i, j] <- 999  # Penalize backward transitions
      }
    }
  }
  
  pts <- list()  # Store PTS trajectories
  for (i in 1:nreps) {
    repeat {
      # Sample indices ensuring the presence of both start (earliest stage and age) and end (endclass) stages
      mysamp <- sample(1:nrow(features), sampsize, replace = TRUE)
      if (is.element(0, stage[mysamp]) & is.element(endclass, stage[mysamp])) {
        break
      }
    }
    
    dunesamp <- features[mysamp, ]  # Sampled features
    dissamp <- cons[mysamp, mysamp]  # Constrained distance matrix
    
    # Build minimum spanning tree (MST)
    mode(dissamp) <- "numeric"
    g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
    datamst <- mst(g)  # Minimum spanning tree
    
    # Map `mysamp` indices to graph vertices
    stage_samp <- stage[mysamp]
    age_samp <- age[mysamp]
    
    # Identify start and end vertices in the sampled graph
    start_vertex <- which(stage_samp == min(stage_samp) & age_samp == min(age_samp))[1]
    end_vertex <- which(stage_samp == max(stage_samp) & age_samp == max(age_samp))[1]
    
    # Ensure `start_vertex` and `end_vertex` are valid
    if (is.na(start_vertex) || is.na(end_vertex)) {
      next  # Skip if no valid start or end points found
    }
    
    # Get shortest path in the MST from start to end
    datshort <- shortest_paths(datamst, start_vertex, end_vertex, mode = "all", output = "both")
    
    # Extract trajectory indices
    ptsind <- datshort$vpath[[1]]
    pts[[i]] <- mysamp[ptsind]
  }
  
  return(pts)
}

# Generate PTS for males and females with both Age and Disease Stage constraints
male_pts_age_stage <- generate_pts_age_stage(male_data[, 1:2], male_data$Age, male_data$Class)
female_pts_age_stage <- generate_pts_age_stage(female_data[, 1:2], female_data$Age, female_data$Class)

### Scatterplots for Male Patients
# Scatterplot of all male patients colored by disease stage
ggplot(male_data, aes(x = X, y = Y, color = factor(Class))) +
  geom_point(size = 3) +
  labs(
    title = "Scatterplot of Male Patients (Colored by Stage)",
    x = "PCA Component 1", y = "PCA Component 2", color = "Class"
  ) +
  theme_minimal()

# Scatterplot with trajectories for male patients
ggplot() +
  geom_point(data = male_data, aes(x = X, y = Y, color = factor(Class)), alpha = 0.6, size = 3) +
  lapply(male_pts_age_stage, function(pt) {
    geom_path(data = male_data[pt, ], aes(x = X, y = Y), color = "black", alpha = 0.4, linewidth = 0.1)
  }) +
  labs(
    title = "PTS Trajectories for Male Patients (Age and Stage Constraints)",
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
  lapply(female_pts_age_stage, function(pt) {
    geom_path(data = female_data[pt, ], aes(x = X, y = Y), color = "black", alpha = 0.4, linewidth = 0.1)
  }) +
  labs(
    title = "PTS Trajectories for Female Patients (Age and Stage Constraints)",
    x = "PCA Component 1", y = "PCA Component 2", color = "Class"
  ) +
  theme_minimal()

