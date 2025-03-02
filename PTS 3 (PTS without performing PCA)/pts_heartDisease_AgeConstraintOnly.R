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

# Define constraints for age progression
# Only allow forward movement in age
sampsize <- nrow(features)
print(sampsize)
dis <- as.matrix(dist(features, method = "euclidean"))
cons <- dis  # Initialize constraints

for (i in 1:sampsize) {
  for (j in 1:sampsize) {
    if (age[j] < age[i]) {
      cons[i, j] <- 999  # Penalize backward transitions in age
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
  
  # Get shortest path from youngest (minimum age) to oldest (maximum age)
  startp <- which(age[mysamp] == min(age[mysamp]))[1]
  endp <- which(age[mysamp] == max(age[mysamp]))[1]
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

# Ensure FULLPTS has appropriate column names for consistency
colnames(FULLPTS)[1:3] <- c("t", "c", "a")

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

############ A: Visualization: Density Plot for Age Distribution by Disease Stage
ggplot(FULLPTS, aes(x = a, fill = factor(c))) +
  geom_density(alpha = 0.5) +  # Adjust alpha for transparency
  labs(title = "Density Plot of Age Distribution by Disease Stage",
       x = "Age", y = "Density", fill = "Disease Stage") +
  theme_minimal()

# Extract and visualize age progression for the first 5 pseudo-time series paths
pts_data <- data.frame()

for (i in 1:5) {
  path_indices <- pts[[i]]
  age_values <- data$Age[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(age_values),
    Age = age_values,
    Path = factor(rep(i, length(age_values)))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

ggplot(pts_data, aes(x = PseudoTime, y = Age)) +
  geom_line(color = "blue") +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Age Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Age") +
  theme_minimal()

# Explore correlations and visualize relationships between age and disease stage
correlation <- cor(FULLPTS$a, FULLPTS$c, method = "pearson")
print(paste("Pearson correlation between Age and Disease Stage:", correlation))

ggplot(FULLPTS, aes(x = c, y = a)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Disease Stage",
       x = "Disease Stage", y = "Age") +
  theme_minimal()



############ B: Visualization: Disease stages in created PTS 
# Extract and visualize stage progression for the first 5 pseudo-time series paths
pts_data <- data.frame()

for (i in 1:5) {
  path_indices <- pts[[i]]
  target_values <- data$num[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values)))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

ggplot(pts_data, aes(x = PseudoTime, y = Stage)) +
  geom_line(color = "blue") +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Disease Stage Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Age") +
  theme_minimal()

# Explore correlations and visualize relationships between age and disease stage
correlation <- cor(FULLPTS$a, FULLPTS$c, method = "pearson")
print(paste("Pearson correlation between Age and Disease Stage:", correlation))

ggplot(FULLPTS, aes(x = c, y = a)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Disease Stage",
       x = "Disease Stage", y = "Age") +
  theme_minimal()



############ C: Visualization: CHOLESTROL in created PTS 
# Extract and visualize cholestrol progression trends for the first 5 pseudo-time series paths
pts_data <- data.frame()

for (i in 1:5) {
  path_indices <- pts[[i]]
  chol_values <- data$chol[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(chol_values),
    Chol = chol_values,
    Path = factor(rep(i, length(chol_values)))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

ggplot(pts_data, aes(x = PseudoTime, y = Chol)) +
  geom_line(color = "blue") +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Cholestrol Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Age") +
  theme_minimal()

# Explore correlations and visualize relationships between age and disease stage
correlation <- cor(FULLPTS$a, FULLPTS$chol, method = "pearson")
print(paste("Pearson correlation between Age and Cholestrol:", correlation))

ggplot(FULLPTS, aes(x = chol, y = a)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Cholestrol",
       x = "Cholestrol", y = "Age") +
  theme_minimal()


############ D: Visualization: TrestBPS (resting blood pressure) in created PTS 
# Extract and visualize trestbps progression trends for the first 5 pseudo-time series paths
pts_data <- data.frame()

for (i in 1:5) {
  path_indices <- pts[[i]]
  trestbps_values <- data$trestbps[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(trestbps_values),
    TrestBPS = trestbps_values,
    Path = factor(rep(i, length(trestbps_values)))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

ggplot(pts_data, aes(x = PseudoTime, y = TrestBPS)) +
  geom_line(color = "blue") +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Resting Blood Pressure Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Resting Blood Pressure") +
  theme_minimal()

# Explore correlations and visualize relationships between age and disease stage
correlation <- cor(FULLPTS$a, FULLPTS$trestbps, method = "pearson")
print(paste("Pearson correlation between Age and Resting Blood Pressure:", correlation))

ggplot(FULLPTS, aes(x = trestbps, y = Age)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Resting Blood Pressure",
       x = "Resting Blood Pressure", y = "Age") +
  theme_minimal()


############ E: Visualization: Thalach - Maxiumum Heart Rate Achieved in created PTS 
# Extract and visualize thalach progression trends for the first 5 pseudo-time series paths
pts_data <- data.frame()

for (i in 1:5) {
  path_indices <- pts[[i]]
  thalach_values <- data$thalach[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(thalach_values),
    Thalach = thalach_values,
    Path = factor(rep(i, length(thalach_values)))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

ggplot(pts_data, aes(x = PseudoTime, y = Thalach)) +
  geom_line(color = "blue") +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Maximum Heart Rate Achieved Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Maximum Heart Rate Achieved") +
  theme_minimal()

# Explore correlations and visualize relationships between age and disease stage
correlation <- cor(FULLPTS$a, FULLPTS$thalach, method = "pearson")
print(paste("Pearson correlation between Age and Maximum Heart Rate Achieved:", correlation))

ggplot(FULLPTS, aes(x = thalach, y = Age)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Thalach(Maximun Heart Rate Achieved)",
       x = "Maximum Heart Rate Achieved", y = "Age") +
  theme_minimal()



############ F: Visualization: Oldpeak - in created PTS 
# Extract and visualize oldpeak (reduced blood flow) progression trends for the first 5 pseudo-time series paths
pts_data <- data.frame()

for (i in 1:5) {
  path_indices <- pts[[i]]
  oldpeak_values <- data$oldpeak[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(oldpeak_values),
    OldPeak = oldpeak_values,
    Path = factor(rep(i, length(oldpeak_values)))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

ggplot(pts_data, aes(x = PseudoTime, y = OldPeak)) +
  geom_line(color = "blue") +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "OldPeak Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "OldPeak (Reduced BloodFlow to Heart") +
  theme_minimal()

# Explore correlations and visualize relationships between age and disease stage
correlation <- cor(FULLPTS$a, FULLPTS$oldpeak, method = "pearson")
print(paste("Pearson correlation between Age and OldPeak Achieved:", correlation))

ggplot(FULLPTS, aes(x = oldpeak, y = Age)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Oldpeak",
       x = "OldPeak", y = "Age") +
  theme_minimal()



