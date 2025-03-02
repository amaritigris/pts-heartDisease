# Required Libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")
library(dplyr)
library(igraph)
library(ggplot2)

# Read Data
dat <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\PTS 2 (using age and target as constraint)\\pca_transformed_cleveland.csv")
mydata <- dat[, 1:2]  # Extract X and Y coordinates
fullclass <- dat[, 3] # Class (e.g., disease stage)
age <- dat[, 4]       # Age of patients
print(age)
class <- 1 + (fullclass > 0)  # Binary class: 1 = control, 2+ = other stages

# Constraints for Stage Transitions
banfrom <- c(3, 4)  # Disallow transitions from stage 3
banto <- c(4, 3)    # To stage 4
max_age_diff <- 10  # Maximum allowed age difference between transitions

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

# Plot PCA with Pseudo-Time Paths
plot(pcadat$scores[, 1], pcadat$scores[, 2], 
     col = fullclass + 1, 
     pch = 19,
     main = "Scatterplot with PTS paths",
     xlab = "PC1", 
     ylab = "PC2")

# Add pseudo-time trajectories with transparency
for (i in 1:nreps) {
  if (!is.null(pts[[i]])) {
    lines(pcadat$scores[pts[[i]], 1], 
          pcadat$scores[pts[[i]], 2],
          col = rgb(class[pts[[i]]] / max(class), 0, 1 - class[pts[[i]]] / max(class), alpha = 0.5), 
          lwd = 0.1)  # Adjust alpha for transparency and lwd for line thickness
  }
}

# Add a legend
legend("topright", 
       legend = unique(fullclass), 
       col = unique(fullclass + 1), 
       pch = 19, 
       title = "Stages", 
       bty = "n")


# Organize Data for ggplot
FULLPTS <- data.frame()
for (i in 1:nreps) {
  if (!is.null(pts[[i]])) {
    t <- 1:length(pts[[i]])  # Pseudo-time
    c <- fullclass[pts[[i]]]  # Class
    pcas <- pcadat$scores[pts[[i]], 1:2]
    ages <- age[pts[[i]]]     # Age along the path
    IDPTS <- cbind(t, c, pcas, ages)
    FULLPTS <- rbind(FULLPTS, IDPTS)
  }
}
colnames(FULLPTS) <- c("PseudoTime", "Class", "PC1", "PC2", "Age")


# Visualize Age Trends in Pseudo-Time
ggplot(FULLPTS, aes(x = PseudoTime, y = Age, color = factor(Class))) +
  geom_smooth(se = FALSE) +
  labs(title = "Age Trends Over Pseudo-Time",
       x = "Pseudo-Time", y = "Age", color = "Class") +
  theme_minimal()


#saving pts as csv for evaluation purposes
# Convert PTS list to a data frame for saving
pts_df <- do.call(rbind, lapply(1:length(pts), function(i) {
  data.frame(
    Rep = i,  # Identifier for the repetition
    PseudoTime = 1:length(pts[[i]]),  # Time point in the trajectory
    Index = pts[[i]],  # Indices from sampled data
    PC1 = pcadat$scores[pts[[i]], 1],  # PCA Component 1
    PC2 = pcadat$scores[pts[[i]], 2],  # PCA Component 2
    Age = age[pts[[i]]],  # Age of patients
    Class = class[pts[[i]]]  # Disease stage
  )
}))

write.csv(pts_df, "D:\\CS Year 3\\FYP\\PTS code\\pca_generated_pts_ageANDstageConstraints.csv", row.names = FALSE)




