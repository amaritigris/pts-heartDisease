# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(igraph)
library(ggplot2)

#######################################################
## Read in Data
#######################################################

# Load the Cleveland dataset
dat <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\PTS 2 (using age and target as constraint)\\pca_transformed_cleveland.csv", row.names = NULL)
head(dat)

# Extract relevant columns
mydata = dat[, 1:2]  # Assume Age and another feature
head(mydata)
fullclass = dat[, 3]  # Original disease stages

# Build distance matrix
dis <- dist(mydata, method = "euclidean")
dis = as.matrix(dis)
sampsize = nrow(mydata)

#######################################################
## Scatterplot with Original Disease Stages
#######################################################

# Perform PCA for visualization
pcadat = prcomp(mydata, center = TRUE, scale. = TRUE)

# Scatterplot of PCA components colored by original disease stages
plot(pcadat$x[, 1], pcadat$x[, 2], col = as.factor(fullclass), pch = 19,
     xlab = "PCA Component 1", ylab = "PCA Component 2",
     main = "Scatterplot of PCA Components with Original Disease Stages")
legend("topright", legend = unique(fullclass), col = unique(as.numeric(as.factor(fullclass))), pch = 19)

#######################################################
## PTS Algorithm (Using Original Disease Stages)
#######################################################

nreps = 100  # Number of PTS generated
pts = list()  # List to store PTS trajectories

# Generate PTS trajectories
for (i in 1:nreps) {
  repeat {
    mysamp = sample(1:nrow(mydata), sampsize, replace = TRUE)  # Random sample of size sampsize
    if (length(unique(fullclass[mysamp])) > 1) {
      break  # Ensure at least two different disease stages are included in the sample
    }
  }
  
  dunesamp = mydata[mysamp, ]
  classsamp = fullclass[mysamp]
  
  # Randomly choose a start and end point from the sample
  repeat {
    startp = sample(1:sampsize, 1)
    endp = sample(1:sampsize, 1)
    if (fullclass[mysamp[startp]] < fullclass[mysamp[endp]]) {
      break
    }
  }
  
  # Build the distance matrix for the sampled data
  dissamp = dis[mysamp, mysamp]
  
  # Build minimum spanning tree (MST)
  mode(dissamp) <- "numeric"
  g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
  datamst = mst(g)  # Minimum spanning tree
  
  # Get the shortest path from startp to endp
  datshort = shortest_paths(datamst, startp, endp, mode = "all", output = "both")
  
  # Store PTS indices
  ptsind = datshort$vpath[[1]]
  pts[[i]] = mysamp[ptsind]
}

#######################################################
## Overlay PTS Paths on Scatterplot
#######################################################

# Replot the scatterplot
plot(pcadat$x[, 1], pcadat$x[, 2], col = as.factor(fullclass), pch = 19,
     xlab = "PCA Component 1", ylab = "PCA Component 2",
     main = "Scatterplot with PTS Paths")
legend("topright", legend = unique(fullclass), col = unique(as.numeric(as.factor(fullclass))), pch = 19)

# Overlay PTS trajectories
for (i in 1:nreps) {
  points(pcadat$x[pts[[i]], 1], pcadat$x[pts[[i]], 2], type = "l", col = "black", lwd = 1)
}

#saving the created PTS for later evaluation purposes
# Convert PTS list to a data frame for saving
pts_df <- do.call(rbind, lapply(1:length(pts), function(i) {
  data.frame(
    Rep = i,  # Identifier for the repetition
    Index = pts[[i]],  # Indices from sampled data
    PC1 = pcadat$x[pts[[i]], 1],  # PCA Component 1
    PC2 = pcadat$x[pts[[i]], 2],  # PCA Component 2
    Stage = fullclass[pts[[i]]]  # Disease stage
  )
}))
head(pts_df)
write.csv(pts_df, "D:\\CS Year 3\\FYP\\PTS code\\pca_generated_pts_noConstraints.csv", row.names = FALSE)



