# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")

# Load required libraries
library(dplyr)
library(igraph)
library(ggplot2)

#######################################################
## Load the Cleveland dataset from UCI repository
#######################################################

# Download the Cleveland dataset (processed version)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
dat <- read.csv(url, header = FALSE)

# Set column names for the Cleveland dataset
colnames(dat) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

# Preview the dataset
head(dat)

# Classify 'num' (target) into binary: 0 (no heart disease) and 1 (heart disease present)
# Heart disease presence (num > 0) is classified as 1, else 0
class <- dat$num  # Keep the original disease stages
print(class)

# Extract relevant features for progression (using 'age' and other relevant features)
mydata <- dat[, c("age", "trestbps", "chol", "thalach", "oldpeak")]  # Example features
print(mydata)

# No constraints for disease stage progression (no need for 'banfrom' or 'banto')
banfrom <- NULL
banto <- NULL

#######################################################
## PTS Code (No Constraints for Disease Stage or Age)
#######################################################

# Build full distance matrix based on the features
dis <- dist(mydata, method = "euclidean")
dis = as.matrix(dis)
print(dis)

sampsize = nrow(mydata)
print(sampsize)

cons = dis  # No constraints applied, using the direct distance matrix

# Initialize PTS parameters
nreps = 100  # Number of PTS generated
pts = list()  # List to store PTS trajectories

# Generate PTS trajectories
for (i in 1:nreps) {
  repeat {
    mysamp = sample(1:nrow(mydata), sampsize, replace = TRUE)  # Random sample
    if (length(unique(class[mysamp])) > 1) {
      break  # Ensure multiple disease stages are included in the sample
    }
  }
  
  # Randomly choose a start and end point with distinct stages
  repeat {
    startp = sample(1:sampsize, 1)
    endp = sample(1:sampsize, 1)
    if (class[mysamp[startp]] != class[mysamp[endp]]) {
      break  # Start and end points must have distinct disease stages
    }
  }
  # Build the distance matrix for the sampled data (no constraints)
  dissamp = cons[mysamp, mysamp]
  
  # Build minimum spanning tree (MST)
  mode(dissamp) <- "numeric"
  g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
  datamst = mst(g)  # Minimum spanning tree
  
  # Get the shortest path from class 1 (control) to class 2 (disease)
  datshort = shortest_paths(datamst, startp, endp, mode = "all", output = "both")
  
  # Store PTS indices
  ptsind = datshort$vpath[[1]]
  pts[[i]] = mysamp[ptsind]
}

#######################################################
## Visualization of Disease Progression Using Age
#######################################################
FULLPTS = data.frame()
for (i in 1:nreps) {
  t = 1:length(pts[[i]])  # Pseudo-time
  c = class[pts[[i]]]  # Use original multi-stage classes
  a = mydata[pts[[i]], 1]  # Age (first column of mydata)
  features = mydata[pts[[i]], 2:5]  # Additional features
  
  # Combine pseudo-time, disease stage, and features
  IDPTS = cbind(t, c, a, features)
  FULLPTS = rbind(FULLPTS, IDPTS)
}

# Combine PTS data for visualization
pts_combined <- do.call(rbind, lapply(1:5, function(i) {
  pts_df <- dat[pts[[i]], ]  # Get the data for PTS i
  pts_df$pts <- paste0("PTS ", i)  # Label the PTS
  return(pts_df)
}))

# Bar plot for 'num'
ggplot(pts_combined, aes(x = as.factor(num), fill = pts)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Heart Disease Severity (num) Across PTS",
       x = "Heart Disease Severity (num)", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Combine PTS data for pseudo-time and age visualization
#we can change the age variable to other features as well to see what chnages in those features for each PTS
#for my case study i did - age, chol, thalach, oldpeak
FULLPTS <- data.frame()
for (i in 1:5) {
  t <- 1:length(pts[[i]])  # Pseudo-time
  oldpeak <- mydata[pts[[i]], "oldpeak"]  # Cholestrol
  pts_label <- rep(paste0("PTS ", i), length(t))  # PTS label
  temp_df <- data.frame(PseudoTime = t, OldPeak = oldpeak, PTS = pts_label)
  FULLPTS <- rbind(FULLPTS, temp_df)
}

# Add PTS information to FULLPTS for individual plotting
FULLPTS$PTS <- factor(FULLPTS$PTS, levels = unique(FULLPTS$PTS))

# Plot each PTS on a separate panel using facet_wrap
ggplot(FULLPTS, aes(x = PseudoTime, y = OldPeak, color = PTS, group = PTS)) +
  geom_line(size = 1) +  # Thicker lines for better readability
  labs(title = "OldPeak (Less than normal blood flow) Progression Across Pseudo-time",
       x = "Pseudo-time", y = "Age") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ PTS, ncol = 1)  # Create separate plots for each PTS








