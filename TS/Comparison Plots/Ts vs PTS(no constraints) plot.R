# Install the dplyr package (if not already installed)
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("igraph")) install.packages("igraph")



# Load the dplyr package
library(dplyr)
library(ggplot2)
library(igraph)



#read the time series data file
pca_data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\TS\\pca_transformed_time_series.csv", na.strings="?")
head(pca_data)

# Convert Stage to factor for better visualization
pca_data$Stage <- as.factor(pca_data$Stage)

#select 300 unique patients 
set.seed(123) #ensure reproducibility
selected_patients <- sample(unique(pca_data$STUDYNO), 30)

# Filter dataset to only include the selected patients
pca_sampled <- pca_data %>% filter(STUDYNO %in% selected_patients)

#read the pca file which is meant to create psuedo time series (cleveleand dataset)
dat <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\PTS 2 (using age and target as constraint)\\pca_transformed_cleveland.csv", row.names = NULL)

# Extract relevant columns
mydata = dat[, 1:2]  # Assume Age and another feature
fullclass = dat[, 3]  # Original disease stages
STUDYNO = dat[, 4]  # Patient ID (Assumed column)

# Build distance matrix
dis <- dist(mydata, method = "euclidean")
dis = as.matrix(dis)

sampsize = nrow(mydata)

pcadat = prcomp(mydata, center = TRUE, scale. = TRUE)
pcadat$x[, 1] <- -pcadat$x[, 1]  # Flip PCA Component 1


nreps = 100  # Number of PTS generated
pts = list()  # Store PTS trajectories

for (i in 1:nreps) {
  repeat {
    mysamp = sample(1:nrow(mydata), sampsize, replace = TRUE)  
    if (length(unique(fullclass[mysamp])) > 1) {
      break
    }
  }
  
  dunesamp = mydata[mysamp, ]
  classsamp = fullclass[mysamp]
  
  repeat {
    startp = sample(1:sampsize, 1)
    endp = sample(1:sampsize, 1)
    if (fullclass[mysamp[startp]] < fullclass[mysamp[endp]]) {
      break
    }
  }
  
  dissamp = dis[mysamp, mysamp]
  
  mode(dissamp) <- "numeric"
  g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
  datamst = mst(g)
  
  datshort = shortest_paths(datamst, startp, endp, mode = "all", output = "both")
  
  ptsind = datshort$vpath[[1]]
  pts[[i]] = mysamp[ptsind]
}

# Prepare a pseudo-time series dataset for ggplot
pseudo_df <- data.frame(
  PC1 = pcadat$x[unlist(pts), 1], 
  PC2 = pcadat$x[unlist(pts), 2], 
  Stage = as.factor(fullclass[unlist(pts)]) # Assign disease stage to pseudo-time points
)


ggplot() +
  # Plot real time series (colored by disease stage)
  geom_point(data = pca_sampled, aes(x = PC1, y = PC2, color = Stage), size = 3, alpha = 0.7) +
  geom_line(data = pca_sampled, aes(x = PC1, y = PC2, group = STUDYNO, color = Stage), alpha = 0.5) + 
  
  # Overlay pseudo-time series paths (black lines for structure)
  geom_path(data = pseudo_df, aes(x = PC1, y = PC2, group = Stage), 
            color = "black", size = 0.7, alpha = 0.6) +
  
  # Add pseudo-time series data points (colored triangles)
  geom_point(data = pseudo_df, aes(x = PC1, y = PC2, color = Stage), 
             size = 2.5, shape = 17) +  # Shape 17 = Triangle
  
  # Define colors for disease stages (same as real time series)
  scale_color_manual(name = "Disease Stage",
                     values = c("0" = "blue", "1" = "darkgreen", "2" = "brown", 
                                "3" = "orange", "4" = "red")) +
  
  # Labels & Theme
  labs(title = "Comparison of Real vs. Pseudo-Time Series",
       x = "PCA Component 1", y = "PCA Component 2") +
  theme_minimal()
