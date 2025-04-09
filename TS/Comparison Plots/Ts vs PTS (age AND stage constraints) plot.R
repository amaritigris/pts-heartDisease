# Install required packages if not installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("igraph")) install.packages("igraph")
if (!require("viridis")) install.packages("viridis")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(igraph)
library(viridis)

# Read the real time series PCA data
pca_data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\TS\\pca_transformed_time_series.csv", na.strings="?")

# Convert Stage to factor for better visualization
pca_data$Stage <- as.factor(pca_data$Stage)

# Select a random subset of 30 unique patients
set.seed(123)  # Ensure reproducibility
selected_patients <- sample(unique(pca_data$STUDYNO), 30)

# Filter dataset to only include selected patients
pca_sampled <- pca_data %>% filter(STUDYNO %in% selected_patients)

# Read the PCA file for pseudo-time series (Cleveland dataset)
dat <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\PTS 2 (using age and target as constraint)\\pca_transformed_cleveland.csv", row.names = NULL)

mydata <- dat[, 1:2]  # Extract X and Y coordinates
fullclass <- dat[, 3] # Class (e.g., disease stage)
age <- dat[, 4]       # Age of patients
print(age)
class <- 1 + (fullclass > 0)  # Binary class: 1 = control, 2+ = other stages

# Constraints for Stage Transitions
banfrom <- c(3, 4)  # Disallow transitions from stage 3
banto <- c(4, 3)    # To stage 4
max_age_diff <- 5  # Maximum allowed age difference between transitions

# Build Distance Matrix
dis <- as.matrix(dist(mydata, method = "euclidean"))

# Apply Constraints (Stage and Age)
cons <- dis
sampsize <- nrow(mydata)
for (i in 1:sampsize) {
  for (j in 1:sampsize) {
    for (k in 1:length(banfrom)) {
      if ((fullclass[i] == banfrom[k]) & (fullclass[j] == banto[k])) {
        cons[i, j] <- 999  # Apply stage transition constraint
        cons[j, i] <- 999
      }
    }
    # Apply age constraint
    if (abs(age[i] - age[j]) > max_age_diff) {
      cons[i, j] <- 999  # Penalize age-distant transitions
      cons[j, i] <- 999
    }
  }
}

# PCA for Dimensionality Reduction
pcadat <- princomp(mydata)
plot(pcadat$scores[, 1], pcadat$scores[, 2], col = as.numeric(class) + 1, pch = 19,
     main = "PCA Plot (Color by Class)")

# Generate Pseudo-Time Paths
sampsize <- 50       # Length of pseudo-time paths
nreps <- 100         # Number of paths to generate
endclass <- 2        # End class (target stage)
pts <- list()        # Store paths

for (i in 1:nreps) {
  # Resample Data
  repeat {
    mysamp <- sample(1:nrow(mydata), sampsize, replace = TRUE)
    if (is.element(1, class[mysamp]) & is.element(endclass, class[mysamp])) break
  }
  dunesamp <- mydata[mysamp, ]
  
  # Randomly Select Start and End Points
  repeat {
    startp <- sample(1:sampsize, 1)
    endp <- sample(1:sampsize, 1)
    if ((class[mysamp[startp]] == 1) && (class[mysamp[endp]] == endclass)) break
  }
  
  # Build Minimum Spanning Tree
  dissamp <- cons[mysamp, mysamp]
  mode(dissamp) <- "numeric"
  g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
  datamst <- mst(g)
  
  # Get Shortest Path
  datshort <- shortest_paths(datamst, startp, endp, mode = "all", output = "both")
  ptsind <- datshort$vpath
  pts[[i]] <- mysamp[ptsind[[1]]]
}

# Convert PCA data for visualization
pca_transformed <- data.frame(
  PC1 = features[,1], 
  PC2 = features[,2], 
  DiseaseStage = as.factor(class)
)

# Convert PTS trajectories into a data frame for ggplot
pts_df <- do.call(rbind, lapply(1: function(i) {  # Select only 20 PTS for clarity
  data.frame(
    PC1 = features[pts[[i]], 1],
    PC2 = features[pts[[i]], 2],
    DiseaseStage = as.factor(class[pts[[i]]]),
    Trajectory = as.factor(i)  # Assign a trajectory ID
  )
}))

pts_df$PC1 <- -pts_df$PC1
pts_df$PC2 <- -pts_df$PC2
# Plot TS and PTS
ggplot() +
  # Plot real time series (colored by disease stage)
  geom_point(data = pca_sampled, aes(x = PC1, y = PC2, color = Stage), size = 3, alpha = 0.7) +
  geom_line(data = pca_sampled, aes(x = PC1, y = PC2, group = STUDYNO, color = Stage), alpha = 0.5) + 
  
  # Overlay pseudo-time series paths (black lines for structure)
  geom_path(data = pts_df, aes(x = PC1, y = PC2, group = Trajectory), 
            color = "black", size = 0.7, alpha = 0.6) +
  
  # Add pseudo-time series data points (colored triangles)
  geom_point(data = pts_df, aes(x = PC1, y = PC2, color = DiseaseStage), 
             size = 2.5, shape = 17) +  # Shape 17 = Triangle
  
  # Define colors for disease stages (consistent with real time series)
  scale_color_manual(name = "Disease Stage",
                     values = c("0" = "blue", "1" = "darkgreen", "2" = "brown", 
                                "3" = "orange", "4" = "red")) +
  
  # Labels & Theme
  labs(title = "Comparison of Real vs. Pseudo-Time Series (Age and Stage Constraints)",
       x = "PCA Component 1", y = "PCA Component 2") +
  theme_minimal()

