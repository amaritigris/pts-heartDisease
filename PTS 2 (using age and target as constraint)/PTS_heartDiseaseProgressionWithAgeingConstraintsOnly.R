# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(igraph)
library(ggplot2)

# Load the dataset
data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\PTS 2 (using age and target as constraint)\\pca_transformed_cleveland.csv", row.names = NULL)
head(data)

# Extract relevant columns
features <- data[, 1:2]  # PCA components (first two columns)
age <- data$Age  # Age column
target <- data$Class  # Disease severity column (assumed to be "num")

# Define disease stages (0 = no disease, 1-4 = increasing severity)
class <- target
print(class)

# Distance matrix and constraints
sampsize <- nrow(features)
dis <- as.matrix(dist(features, method = "euclidean"))
cons <- dis  # Initialize constraint matrix

# Apply constraints to penalize backward transitions in age
for (i in 1:sampsize) {
  for (j in 1:sampsize) {
    if (age[j] < age[i]) {
      cons[i, j] <- 999  # Penalize backward age transitions
    }
  }
}

# Initialize PTS parameters
nreps <- 100  # Number of PTS trajectories to generate
sampsize <- 50  # PTS trajectory length
endclass <- 4  # Target end class (severe disease)

pts <- list()  # Store PTS trajectories

# Generate PTS trajectories
print(features)
print(nreps)
for (i in 1:nreps) {
  repeat {
    print("repeating")
    # Sample indices ensuring the presence of both start (youngest) and end (oldest) classes
    mysamp <- sample(1:nrow(features), sampsize, replace = TRUE)
    print(class[mysamp])
    if (is.element(0, class[mysamp]) & is.element(endclass, class[mysamp])) {
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

### Visualization and Analysis

# Perform PCA for visualization
pcadat <- princomp(features)

# Scatterplot of PCA scores colored by class
plot(
  pcadat$scores[, 1], pcadat$scores[, 2], 
  col = class + 1, pch = 19, 
  xlab = "Principal Component 1", 
  ylab = "Principal Component 2", 
  main = "PCA Scores Colored by Class"
)

# Add legend
legend(
  "topright", 
  legend = unique(class), 
  col = unique(class + 1), 
  pch = 19, 
  title = "Disease Stage"
)

# Overlay PTS trajectories on PCA scatterplot with transparency
line_color <- rgb(0, 0, 0, alpha = 0.3)  # Transparent black color

for (i in 1:nreps) {
  lines(
    pcadat$scores[pts[[i]], 1], 
    pcadat$scores[pts[[i]], 2], 
    col = line_color, lwd = 1.5
  )
}

# Prepare data for ggplot visualizations
FULLPTS <- data.frame()
for (i in 1:nreps) {
  t <- 1:length(pts[[i]])
  c <- class[pts[[i]]]
  a <- age[pts[[i]]]
  pcas <- pcadat$scores[pts[[i]], 1:2]
  IDPTS <- cbind(t, c, a, pcas)
  FULLPTS <- rbind(FULLPTS, IDPTS)
}
colnames(FULLPTS) <- c("PseudoTime", "Class", "Age", "PC1", "PC2")

# ggplot visualizations
ggplot(FULLPTS, aes(x = PseudoTime, y = PC2, color = factor(Class))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    title = "PC2 Trend Over Pseudo-Time",
    x = "Pseudo-Time", y = "PC2",
    color = "Class"
  ) +
  theme_minimal()

ggplot(FULLPTS, aes(x = PseudoTime, y = PC1, color = factor(Class))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    title = "PC1 Trend Over Pseudo-Time",
    x = "Pseudo-Time", y = "PC1",
    color = "Class"
  ) +
  theme_minimal()

