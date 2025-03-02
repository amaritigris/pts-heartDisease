# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(igraph)
library(ggplot2)

# Load the dataset
data <- read.csv("D:\\CS Year 3\\FYP\\processed.cleveland.data", header = FALSE)
head(data)

# Define column names based on UCI documentation
colnames(data) <- c("Age", "sex", "cp", "trestbps", "chol", 
                    "fbs", "restecg", "thalach", "exang", 
                    "oldpeak", "slope", "ca", "thal", "num")

# Handle missing values by removing rows with any NA values
data <- na.omit(data)

# Select relevant features (Ensure that they are numeric)
features <- data %>%
  select(Age, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal)  # Exclude `num`

# Check if all columns in 'features' are numeric
features[] <- lapply(features, function(x) as.numeric(as.character(x)))

# Normalize features (only if they are numeric)
features <- as.data.frame(scale(features))

# Extract Age and Target information
age <- data$Age
target <- data$num  # Assuming the target column is "num"

# Define disease stages (0 = no disease, 1-4 for increasing severity)
class <- target  # Keep stages from 0 to 4

# Define constraints for disease progression
# Only allow forward movement in stage
sampsize <- nrow(features)
dis <- as.matrix(dist(features, method = "euclidean"))
cons <- dis  # Initialize constraints

for (i in 1:sampsize) {
  for (j in 1:sampsize) {
    if (class[j] < class[i]) {
      cons[i, j] <- 999  # Penalize backward transitions in stage
    }
  }
}

# Initialize PTS parameters
nreps <- 100
pts <- NULL
sampsize <- 50
endclass <- 4  # Set the end class to 4 (severe disease)

# Generate PTS
for (i in 1:nreps) {
  repeat {
    mysamp <- sample(1:nrow(features), sampsize, replace = TRUE)
    if (is.element(0, class[mysamp]) & is.element(endclass, class[mysamp])) {
      break
    }
  }
  dunesamp <- features[mysamp, ]
  dissamp <- cons[mysamp, mysamp]
  
  # Build minimum spanning tree
  mode(dissamp) <- "numeric"
  g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
  datamst <- mst(g)
  
  # Get shortest path from no disease (class 0) to severe disease (class 4)
  startp <- which(class[mysamp] == 0)[1]
  endp <- which(class[mysamp] == endclass)[1]
  datshort <- shortest_paths(datamst, startp, endp, mode = "all", output = "both")
  
  # Extract PTS indices
  ptsind <- datshort$vpath[[1]]
  pts[i] <- list(mysamp[ptsind])
}

# Aggregate and visualize PTS
FULLPTS <- data.frame()
for (i in 1:nreps) {
  t <- 1:length(pts[[i]])
  c <- class[pts[[i]]]
  a <- age[pts[[i]]]
  features_subset <- features[pts[[i]], ]
  IDPTS <- cbind(t, c, a, features_subset)
  
  FULLPTS <- rbind(FULLPTS, IDPTS)
}

# Compute the average age at each pseudo-time point
head(FULLPTS)
avg_age_pts <- FULLPTS %>%
  group_by(t) %>%
  summarize(Average_Age = mean(a, na.rm = TRUE))  

# Plot trend line for average age over pseudo-time
ggplot(avg_age_pts, aes(x = t, y = Average_Age)) +
  geom_smooth(color = "blue", size = 1.2) +  # Trend line
  #geom_point(color = "red", size = 2) +  # Points for better visualization
  labs(title = "Average Age Progression Over Pseudo-Time",
       x = "Pseudo-Time",
       y = "Average Age") +
  theme_minimal()

# Fit a linear model to get the rate of increase
rate_model <- lm(Average_Age ~ t, data = avg_age_pts)

# Extract the slope (rate of increase)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Age per Pseudo-Time Unit:", rate_of_increase, "\n")



# Ensure FULLPTS has appropriate column names for consistency
colnames(FULLPTS)[1:3] <- c("t", "c", "a")

# Visualization Example: Density Plot for Age Distribution by Disease Stage
ggplot(FULLPTS, aes(x = a, fill = factor(c))) +
  geom_density(alpha = 0.5) +  # Adjust alpha for transparency
  labs(title = "Density Plot of Age Distribution by Disease Stage",
       x = "Age", y = "Density", fill = "Disease Stage") +
  theme_minimal()


#3. after this we want to see how age progresses inidividually in each of the time series that have been created 
# we will look at the first 5 pseudo time series and plot different progressions of diffrent features to see what changes in the patients that causes increase in stage

#A - Age
# Initialize an empty data frame to store the extracted data for all 5 paths
pts_data <- data.frame()

# Loop through the first 5 paths (pts[[1]] to pts[[5]]) to extract age and pseudo-time
for (i in 1:5) {
  # Get the indices of samples for the i-th path
  path_indices <- pts[[i]]
  
  # Extract the age values for this path
  age_values <- data$Age[path_indices]
  
  # Create a temporary data frame for this path
  temp_df <- data.frame(
    PseudoTime = 1:length(age_values),  # Pseudo-time (time step)
    Age = age_values,  # Age values along the path
    Path = factor(rep(i, length(age_values)))  # Path index for faceting
  )
  
  # Append the data to pts_data
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2 with facets
ggplot(pts_data, aes(x = PseudoTime, y = Age)) +
  geom_line(color = "blue") +  # Raw line plot for age progression
  facet_wrap(~Path, scales = "free_y", ncol = 1) +  # Faceting by Path
  labs(title = "Raw Age Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Age") +
  theme_minimal()


ggplot(FULLPTS, aes(x = c, y = Age)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear trendline
  labs(title = "Relationship Between Age and Disease Stage",
       x = "Disease Stage",
       y = "Maximum hearr rate achieved") +
  theme_minimal()

# Check correlation between oldpeak and disease stage (c)
correlation <- cor(FULLPTS$Age, FULLPTS$c, method = "pearson")  # Pearson correlation
print(paste("Pearson correlation between Age and disease stage:", correlation))


#B - trestbps
# Initialize an empty data frame to store the extracted data for all 5 paths
pts_data <- data.frame()

# Loop through the first 5 paths (pts[[1]] to pts[[5]]) to extract age and pseudo-time
for (i in 1:5) {
  # Get the indices of samples for the i-th path
  path_indices <- pts[[i]]
  
  # Extract the age values for this path
  trestbps_values <- data$trestbps[path_indices]
  
  # Create a temporary data frame for this path
  temp_df <- data.frame(
    PseudoTime = 1:length(trestbps_values),  # Pseudo-time (time step)
    TrestBP = trestbps_values,  # Age values along the path
    Path = factor(rep(i, length(trestbps_values)))  # Path index for faceting
  )
  
  # Append the data to pts_data
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2 with facets
ggplot(pts_data, aes(x = PseudoTime, y = TrestBP)) +
  geom_line(color = "blue") +  # Raw line plot for age progression
  facet_wrap(~Path, scales = "free_y", ncol = 1) +  # Faceting by Path
  labs(title = "Raw trestbp (Resting Blood Pressure) Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Resting Blood Pressure") +
  theme_minimal()

ggplot(FULLPTS, aes(x = c, y = trestbps)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear trendline
  labs(title = "Relationship Between Resting Blood pressure and Disease Stage",
       x = "Disease Stage",
       y = "Maximum hearr rate achieved") +
  theme_minimal()

# Check correlation between oldpeak and disease stage (c)
correlation <- cor(FULLPTS$trestbps, FULLPTS$c, method = "pearson")  # Pearson correlation
print(paste("Pearson correlation between Resting blood pressure and disease stage:", correlation))



#C - Cholestrol
# Initialize an empty data frame to store the extracted data for all 5 paths
pts_data <- data.frame()

# Loop through the first 5 paths (pts[[1]] to pts[[5]]) to extract age and pseudo-time
for (i in 1:5) {
  # Get the indices of samples for the i-th path
  path_indices <- pts[[i]]
  
  # Extract the age values for this path
  chol_values <- data$chol[path_indices]
  
  # Create a temporary data frame for this path
  temp_df <- data.frame(
    PseudoTime = 1:length(chol_values),  # Pseudo-time (time step)
    Chol = chol_values,  # Age values along the path
    Path = factor(rep(i, length(chol_values)))  # Path index for faceting
  )
  
  # Append the data to pts_data
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2 with facets
ggplot(pts_data, aes(x = PseudoTime, y = Chol)) +
  geom_line(color = "blue") +  # Raw line plot for age progression
  facet_wrap(~Path, scales = "free_y", ncol = 1) +  # Faceting by Path
  labs(title = "Cholestrol Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "cholestrol") +
  theme_minimal()

ggplot(FULLPTS, aes(x = c, y = chol)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear trendline
  labs(title = "Relationship Between Cholestrol and Disease Stage",
       x = "Disease Stage",
       y = "Cholestrol") +
  theme_minimal()

# Check correlation between oldpeak and disease stage (c)
correlation <- cor(FULLPTS$chol, FULLPTS$c, method = "pearson")  # Pearson correlation
print(paste("Pearson correlation between cholestrol and disease stage:", correlation))


#D - Thalach- maximum heart rate achieved
# Initialize an empty data frame to store the extracted data for all 5 paths
pts_data <- data.frame()

# Loop through the first 5 paths (pts[[1]] to pts[[5]]) to extract age and pseudo-time
for (i in 1:5) {
  # Get the indices of samples for the i-th path
  path_indices <- pts[[i]]
  
  # Extract the age values for this path
  thalach_values <- data$thalach[path_indices]
  
  # Create a temporary data frame for this path
  temp_df <- data.frame(
    PseudoTime = 1:length(thalach_values),  # Pseudo-time (time step)
    Thalach = thalach_values,  # Age values along the path
    Path = factor(rep(i, length(thalach_values)))  # Path index for faceting
  )
  
  # Append the data to pts_data
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2 with facets
ggplot(pts_data, aes(x = PseudoTime, y = Thalach)) +
  geom_line(color = "blue") +  # Raw line plot for age progression
  facet_wrap(~Path, scales = "free_y", ncol = 1) +  # Faceting by Path
  labs(title = "Maximum Heart Rate achieved Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "cholestrol") +
  theme_minimal()

ggplot(FULLPTS, aes(x = c, y = thalach)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear trendline
  labs(title = "Relationship Between Thalach and Disease Stage",
       x = "Disease Stage",
       y = "Maximum hearr rate achieved") +
  theme_minimal()

# Check correlation between oldpeak and disease stage (c)
correlation <- cor(FULLPTS$thalach, FULLPTS$c, method = "pearson")  # Pearson correlation
print(paste("Pearson correlation between thalach and disease stage:", correlation))


#E - Old Peak- maximum heart rate achieved
# Initialize an empty data frame to store the extracted data for all 5 paths
pts_data <- data.frame()

# Loop through the first 5 paths (pts[[1]] to pts[[5]]) to extract age and pseudo-time
for (i in 1:5) {
  # Get the indices of samples for the i-th path
  path_indices <- pts[[i]]
  
  # Extract the age values for this path
  oldpeak_values <- data$oldpeak[path_indices]
  
  # Create a temporary data frame for this path
  temp_df <- data.frame(
    PseudoTime = 1:length(oldpeak_values),  # Pseudo-time (time step)
    OldPeak = oldpeak_values,  # Age values along the path
    Path = factor(rep(i, length(oldpeak_values)))  # Path index for faceting
  )
  
  # Append the data to pts_data
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2 with facets
ggplot(pts_data, aes(x = PseudoTime, y = OldPeak)) +
  geom_line(color = "blue") +  # Raw line plot for age progression
  facet_wrap(~Path, scales = "free_y", ncol = 1) +  # Faceting by Path
  labs(title = "Oldpeak (Magnitude of Ischemia - (reduced bloodflow to heart muscles)) for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "oldpeak") +
  theme_minimal()


ggplot(FULLPTS, aes(x = c, y = oldpeak)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear trendline
  labs(title = "Relationship Between Oldpeak and Disease Stage",
       x = "Disease Stage",
       y = "Oldpeak") +
  theme_minimal()

# Check correlation between oldpeak and disease stage (c)
correlation <- cor(FULLPTS$oldpeak, FULLPTS$c, method = "pearson")  # Pearson correlation
print(paste("Pearson correlation between oldpeak and disease stage:", correlation))



