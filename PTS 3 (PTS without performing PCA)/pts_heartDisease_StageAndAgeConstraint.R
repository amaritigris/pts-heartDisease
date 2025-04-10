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
head(data)

# Handle missing values by removing rows with any NA values
data <- na.omit(data)

# Select relevant features (Ensure that they are numeric)
features <- data %>%
  select(Age, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal)  # Exclude `num`

# Check if all columns in 'features' are numeric
print(sapply(features, class))  # This will print the class of each column

# Convert any non-numeric columns to numeric (if necessary)
features[] <- lapply(features, function(x) as.numeric(as.character(x)))

# Normalize features (only if they are numeric)
features <- as.data.frame(scale(features))  # Now features should be numeric and ready to scale
print(features)

# Extract Age and Target information
age <- data$Age
target <- data$num  # Assuming the target column is "num"

# Define disease stages (0 = no disease, 1-4 for increasing severity)
class <- target  # Keep stages from 0 to 4

# Define constraints for disease progression
# Only allow forward movement in both stage and age
sampsize <- nrow(features)
dis <- as.matrix(dist(features, method = "euclidean"))
cons <- dis  # Initialize constraints

for (i in 1:sampsize) {
  for (j in 1:sampsize) {
    if (class[j] < class[i] && age[j] < age[i]) {
      cons[i, j] <- 999  # Penalize backward transitions in stage or age
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
# Initialize an empty dataframe to store extracted PTS data
#FULLPTS_corrected <- data.frame()

# Loop through the first 100 PTS and extract corresponding rows from `data`
#for (i in 1:100) {
#  if (i > length(pts)) break  # Ensure we don't exceed available PTS
  
#  t <- 1:length(pts[[i]])  # Time index
#  c <- class[pts[[i]]]  # Stage
#  a <- age[pts[[i]]]  # Age
  
  # Extract corresponding data rows
#  pts_data <- data[pts[[i]], c("chol", "trestbps")]
  
  # Combine into a structured dataframe
 # IDPTS <- cbind(t, c, a, pts_data)
  
  # Append to the full dataset
 # FULLPTS_corrected <- rbind(FULLPTS_corrected, IDPTS)
#}

# Save to CSV file
#write.csv(FULLPTS_corrected, "D:\\CS Year 3\\FYP\\pseudo_time_series_AgeAndStageConstraints.csv", row.names = FALSE)

# Initialize an empty dataframe to store extracted PTS data
FULLPTS_corrected <- data.frame()

# Loop through the first 100 PTS and extract corresponding rows from `data`
for (i in 1:100) {
  if (i > length(pts)) break  # Ensure we don't exceed available PTS
  
  t <- 1:length(pts[[i]])  # Time index
  c <- class[pts[[i]]]  # Stage
  a <- age[pts[[i]]]  # Age
  
  # Extract corresponding data rows
  pts_data <- data[pts[[i]], c("chol", "trestbps")]
  
  # Combine into a structured dataframe
  IDPTS <- cbind(t, c, a, pts_data)
  
  # Append to the full dataset
  FULLPTS_corrected <- rbind(FULLPTS_corrected, IDPTS)
}

# Save to CSV file
#write.csv(FULLPTS_corrected, "D:\\CS Year 3\\FYP\\pseudo_time_series_ageConstraints.csv", row.names = FALSE)

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

# Compute the average cholesterol at each pseudo-time point
head(FULLPTS)
avg_chol_pts <- FULLPTS %>%
  group_by(t) %>%
  summarize(Average_Chol = mean(chol, na.rm = TRUE))  

# Plot trend line for average cholesterol over pseudo-time
ggplot(avg_chol_pts, aes(x = t, y = Average_Chol)) +
  geom_smooth(color = "blue", size = 1.2) +  # Trend line
  #geom_point(color = "red", size = 2) +  # Points for better visualization
  labs(title = "Average Cholesterol Progression Over Pseudo-Time",
       x = "Pseudo-Time",
       y = "Average Cholesterol") +
  theme_minimal()

# Fit a linear model to get the rate of increase
rate_model <- lm(Average_Chol ~ t, data = avg_chol_pts)

# Extract the slope (rate of increase)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Cholesterol per Pseudo-Time Unit:", rate_of_increase, "\n")

# Compute the average pressure at each pseudo-time point
head(FULLPTS)
avg_pres_pts <- FULLPTS %>%
  group_by(t) %>%
  summarize(Average_Pressure = mean(trestbps, na.rm = TRUE))  

# Plot trend line for average cholesterol over pseudo-time
ggplot(avg_pres_pts, aes(x = t, y = Average_Pressure)) +
  geom_smooth(color = "blue", size = 1.2) +  # Trend line
  #geom_point(color = "red", size = 2) +  # Points for better visualization
  labs(title = "Average Pressure Progression Over Pseudo-Time",
       x = "Pseudo-Time",
       y = "Average Pressure") +
  theme_minimal()

# Fit a linear model to get the rate of increase
rate_model <- lm(Average_Pressure ~ t, data = avg_pres_pts)

# Extract the slope (rate of increase)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Pressure per Pseudo-Time Unit:", rate_of_increase, "\n")

# Compute the average Stage at each pseudo-time point
head(FULLPTS)
avg_stage_pts <- FULLPTS %>%
  group_by(t) %>%
  summarize(Average_Stage = mean(num, na.rm = TRUE))  

# Plot trend line for average cholesterol over pseudo-time
ggplot(avg_stage_pts, aes(x = t, y = Average_Stage)) +
  geom_smooth(color = "blue", size = 1.2) +  # Trend line
  #geom_point(color = "red", size = 2) +  # Points for better visualization
  labs(title = "Average Stage Progression Over Pseudo-Time",
       x = "Pseudo-Time",
       y = "Average Stage") +
  theme_minimal()

# Fit a linear model to get the rate of increase
rate_model <- lm(Average_Stage ~ t, data = avg_stage_pts)

# Extract the slope (rate of increase)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Stages per Pseudo-Time Unit:", rate_of_increase, "\n")


# Plotting
# 1. Density Plot for Age Distribution by Disease Stage
ggplot(FULLPTS, aes(x = a, fill = factor(c))) +
  geom_density(alpha = 0.5) +  # Adjust alpha for transparency
  labs(title = "Density Plot of Age Distribution by Disease Stage",
       x = "Age", y = "Density", fill = "Disease Stage") +
  theme_minimal()

# Ensure FULLPTS has appropriate column names for consistency
#in this case t = pseudo time, c = disease stage, a = age
colnames(FULLPTS)[1:3] <- c("t", "c", "a")

# Plot Stage-Wise Progression of Age Over Pseudo-Time
ggplot(FULLPTS, aes(x = t, y = a, color = factor(c), group = c)) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  facet_wrap(~ c, scales = "free_y") +  # Create separate panels for each stage
  labs(title = "Stage-Wise Progression of Age Over Pseudo-Time",
       x = "Pseudo-Time", y = "Age", color = "Disease Stage") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since stages are clear in facets


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

#A - Stage
# Initialize an empty data frame to store the extracted data for all 5 paths
pts_data <- data.frame()

# Loop through the first 5 paths (pts[[1]] to pts[[5]]) to extract age and pseudo-time
for (i in 1:5) {
  # Get the indices of samples for the i-th path
  path_indices <- pts[[i]]
  
  # Extract the age values for this path
  stage_values <- data$num[path_indices]
  
  # Create a temporary data frame for this path
  temp_df <- data.frame(
    PseudoTime = 1:length(stage_values),  # Pseudo-time (time step)
    Stage = stage_values,  # Age values along the path
    Path = factor(rep(i, length(stage_values)))  # Path index for faceting
  )
  
  # Append the data to pts_data
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2 with facets
ggplot(pts_data, aes(x = PseudoTime, y = Stage)) +
  geom_line(color = "blue") +  # Raw line plot for age progression
  facet_wrap(~Path, scales = "free_y", ncol = 1) +  # Faceting by Path
  labs(title = "Raw Stage Progression for First 5 Pseudo-Time Paths",
       x = "Pseudo-Time", y = "Stage") +
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

# Explore correlations and visualize relationships between age and Pressure
correlation <- cor(FULLPTS$a, FULLPTS$trestbps, method = "pearson")
print(paste("Pearson correlation between Age and Blood Pressure:", correlation))

ggplot(FULLPTS, aes(x = trestbps, y = a)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Pressure",
       x = "Pressure", y = "Age") +
  theme_minimal()

# Explore correlations and visualize relationships between Stage and Pressure
correlation <- cor(FULLPTS$c, FULLPTS$trestbps, method = "pearson")
print(paste("Pearson correlation between Stage and Pressure:", correlation))

ggplot(FULLPTS, aes(x = trestbps, y = c)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Stage and Pressure",
       x = "Pressure", y = "Stage") +
  theme_minimal()


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

# Explore correlations and visualize relationships between age and Cholesterol
correlation <- cor(FULLPTS$a, FULLPTS$chol, method = "pearson")
print(paste("Pearson correlation between Age and Cholestrol:", correlation))

ggplot(FULLPTS, aes(x = chol, y = a)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Cholestrol",
       x = "Cholestrol", y = "Age") +
  theme_minimal()

# Explore correlations and visualize relationships between Stage and Cholesterol
correlation <- cor(FULLPTS$c, FULLPTS$chol, method = "pearson")
print(paste("Pearson correlation between Stage and Cholestrol:", correlation))

ggplot(FULLPTS, aes(x = chol, y = c)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Stage and Cholestrol",
       x = "Cholestrol", y = "Stage") +
  theme_minimal()





