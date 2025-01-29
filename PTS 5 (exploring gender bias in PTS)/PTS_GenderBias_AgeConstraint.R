# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("igraph")) install.packages("igraph")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(igraph)
library(ggplot2)

# Load the dataset
data <- read.csv("D:\\CS Year 3\\FYP\\processed.cleveland.data", header = FALSE)

# Define column names based on UCI documentation
colnames(data) <- c("Age", "sex", "cp", "trestbps", "chol", 
                    "fbs", "restecg", "thalach", "exang", 
                    "oldpeak", "slope", "ca", "thal", "num")

# Handle missing values by removing rows with any NA values
data <- na.omit(data)

# Separate the dataset by gender
male_data <- data %>% filter(sex == 1)  # Male patients
female_data <- data %>% filter(sex == 0)  # Female patients

create_pts <- function(dataset, nreps = 100, sampsize = 50, endclass = 4) {
  features <- dataset %>%
    select(Age, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal)
  
  # Ensure features are numeric and normalized
  features[] <- lapply(features, function(x) as.numeric(as.character(x)))
  features <- as.data.frame(scale(features))
  
  age <- dataset$Age
  target <- dataset$num
  class <- target
  
  dis <- as.matrix(dist(features, method = "euclidean"))
  cons <- dis
  
  for (i in 1:nrow(features)) {
    for (j in 1:nrow(features)) {
      if (age[j] < age[i]) {
        cons[i, j] <- 999  # Penalize backward transitions in age
      }
    }
  }
  
  pts <- list()
  
  for (i in 1:nreps) {
    repeat {
      mysamp <- sample(1:nrow(features), sampsize, replace = TRUE)
      if (is.element(0, class[mysamp]) & is.element(endclass, class[mysamp])) {
        break
      }
    }
    dunesamp <- features[mysamp, ]
    dissamp <- cons[mysamp, mysamp]
    
    mode(dissamp) <- "numeric"
    g <- graph_from_adjacency_matrix(dissamp, weighted = TRUE, mode = "undirected")
    datamst <- mst(g)
    
    startp <- which(age[mysamp] == min(age[mysamp]))[1]
    endp <- which(age[mysamp] == max(age[mysamp]))[1]
    datshort <- shortest_paths(datamst, startp, endp, mode = "all", output = "both")
    
    ptsind <- datshort$vpath[[1]]
    pts[i] <- list(mysamp[ptsind])
  }
  print(pts)
  
  FULLPTS <- data.frame()
  
  for (i in 1:nreps) {
    t <- 1:length(pts[[i]])
    c <- class[pts[[i]]]
    a <- age[pts[[i]]]
    features_subset <- features[pts[[i]], ]
    IDPTS <- cbind(t, c, a, features_subset)
    
    FULLPTS <- rbind(FULLPTS, IDPTS)
  }
  
  colnames(FULLPTS)[1:3] <- c("t", "c", "a")
  
  # Return both pts and FULLPTS as a list
  return(list(raw_pts = pts, full_pts = FULLPTS))
}

# Example usage: Generate PTS for males and females
male_results <- create_pts(male_data)
female_results <- create_pts(female_data)

# Extract raw_pts and full_pts separately
raw_male_pts <- male_results$raw_pts
male_pts <- male_results$full_pts

raw_female_pts <- female_results$raw_pts
female_pts <- female_results$full_pts


# Compare male and female PTS
############ A: Visualization: Density Plot for Age Distribution by Disease Stage
ggplot() +
  geom_density(data = male_pts, aes(x = a, fill = factor(c)), alpha = 0.5, color = "blue") +
  geom_density(data = female_pts, aes(x = a, fill = factor(c)), alpha = 0.5, color = "red") +
  labs(title = "Density Plot of Age Distribution by Disease Stage (Male vs Female)",
       x = "Age", y = "Density", fill = "Disease Stage") +
  theme_minimal() +
  scale_fill_discrete(name = "Disease Stage") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Disease Stage"))

############ B: Disease Stage Progression for Male and Female PTS
# Create a combined data frame for plotting
pts_data <- data.frame()

# Process the first 5 paths for males
for (i in 1:5) {
  path_indices <- raw_male_pts[[i]]
  target_values <- male_data$num[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Male", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Process the first 5 paths for females
for (i in 1:5) {
  path_indices <- raw_female_pts[[i]]
  target_values <- female_data$num[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Female", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2
library(ggplot2)
ggplot(pts_data, aes(x = PseudoTime, y = Stage, color = Gender)) +
  geom_line() +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Disease Stage Progression for First 5 Pseudo-Time Paths (Male vs Female)",
       x = "Pseudo-Time", y = "Stage") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()


############ C: Age Progression for Male and Female PTS
# Create a combined data frame for plotting
pts_data <- data.frame()

# Process the first 5 paths for males
for (i in 1:5) {
  path_indices <- raw_male_pts[[i]]
  target_values <- male_data$Age[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Male", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Process the first 5 paths for females
for (i in 1:5) {
  path_indices <- raw_female_pts[[i]]
  target_values <- female_data$Age[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Female", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2
library(ggplot2)
ggplot(pts_data, aes(x = PseudoTime, y = Stage, color = Gender)) +
  geom_line() +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Age Progression for First 5 Pseudo-Time Paths (Male vs Female)",
       x = "Pseudo-Time", y = "Age") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()



# Explore correlations and visualize relationships between age and disease stage
correlation_male <- cor(male_pts$a, male_pts$c, method = "pearson")
correlation_female <- cor(female_pts$a, female_pts$c, method = "pearson")
print(paste("Pearson correlation between Age and Disease Stage (Male):", correlation_male))
print(paste("Pearson correlation between Age and Disease Stage (Female):", correlation_female))

ggplot() +
  geom_point(data = male_pts, aes(x = c, y = a), alpha = 0.5, color = "blue") +
  geom_point(data = female_pts, aes(x = c, y = a), alpha = 0.5, color = "red") +
  geom_smooth(data = male_pts, aes(x = c, y = a), method = "lm", color = "blue", se = TRUE) +
  geom_smooth(data = female_pts, aes(x = c, y = a), method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Disease Stage (Male vs Female)",
       x = "Disease Stage", y = "Age") +
  theme_minimal()


############ D: Cholestrol Progression for Male and Female PTS
# Create a combined data frame for plotting
pts_data <- data.frame()

# Process the first 5 paths for males
for (i in 1:5) {
  path_indices <- raw_male_pts[[i]]
  target_values <- male_data$chol[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Male", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Process the first 5 paths for females
for (i in 1:5) {
  path_indices <- raw_female_pts[[i]]
  target_values <- female_data$chol[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Female", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2
library(ggplot2)
ggplot(pts_data, aes(x = PseudoTime, y = Stage, color = Gender)) +
  geom_line() +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Cholestrol Progression for First 5 Pseudo-Time Paths (Male vs Female)",
       x = "Pseudo-Time", y = "Cholestrol") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()


#correlation between age and cholestrol 
correlation_male <- cor(male_pts$a, male_pts$chol, method = "pearson")
correlation_female <- cor(female_pts$a, female_pts$chol, method = "pearson")
print(paste("Pearson correlation between Age and Cholestrol (Male):", correlation_male))
print(paste("Pearson correlation between Age and Cholestrol (Female):", correlation_female))

ggplot() +
  geom_point(data = male_pts, aes(x = chol, y = a), alpha = 0.5, color = "blue") +
  geom_point(data = female_pts, aes(x = chol, y = a), alpha = 0.5, color = "red") +
  geom_smooth(data = male_pts, aes(x = chol, y = a), method = "lm", color = "blue", se = TRUE) +
  geom_smooth(data = female_pts, aes(x = chol, y = a), method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Cholestrol (Male vs Female)",
       x = "Cholestrol", y = "Age") +
  theme_minimal()



############ E: Resting Blood Pressure (trestbps) for Male and Female PTS
# Create a combined data frame for plotting
pts_data <- data.frame()

# Process the first 5 paths for males
for (i in 1:5) {
  path_indices <- raw_male_pts[[i]]
  target_values <- male_data$trestbps[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Male", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Process the first 5 paths for females
for (i in 1:5) {
  path_indices <- raw_female_pts[[i]]
  target_values <- female_data$trestbps[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Female", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2
library(ggplot2)
ggplot(pts_data, aes(x = PseudoTime, y = Stage, color = Gender)) +
  geom_line() +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Resting Blood Pressure Progression for First 5 Pseudo-Time Paths (Male vs Female)",
       x = "Pseudo-Time", y = "Resting Blood Pressure") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()


#correlation between age and resting blood pressure 
correlation_male <- cor(male_pts$a, male_pts$trestbps, method = "pearson")
correlation_female <- cor(female_pts$a, female_pts$trestbps, method = "pearson")
print(paste("Pearson correlation between Age and Resting Blood Pressure (Male):", correlation_male))
print(paste("Pearson correlation between Age and Resting Blood Pressure (Female):", correlation_female))

ggplot() +
  geom_point(data = male_pts, aes(x = trestbps, y = a), alpha = 0.5, color = "blue") +
  geom_point(data = female_pts, aes(x = trestbps, y = a), alpha = 0.5, color = "red") +
  geom_smooth(data = male_pts, aes(x = trestbps, y = a), method = "lm", color = "blue", se = TRUE) +
  geom_smooth(data = female_pts, aes(x = trestbps, y = a), method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Resting Blood Pressure (Male vs Female)",
       x = "Resting Blood Pressure", y = "Age") +
  theme_minimal()


############ F: Maximum Heart Rate Achieved (thalach) for Male and Female PTS
# Create a combined data frame for plotting
pts_data <- data.frame()

# Process the first 5 paths for males
for (i in 1:5) {
  path_indices <- raw_male_pts[[i]]
  target_values <- male_data$thalach[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Male", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Process the first 5 paths for females
for (i in 1:5) {
  path_indices <- raw_female_pts[[i]]
  target_values <- female_data$thalach[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Female", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2
library(ggplot2)
ggplot(pts_data, aes(x = PseudoTime, y = Stage, color = Gender)) +
  geom_line() +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Maximum Heart Rate Progression for First 5 Pseudo-Time Paths (Male vs Female)",
       x = "Pseudo-Time", y = "Thalach (Maximum Heart Rate)") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()


#correlation between age and resting blood pressure 
correlation_male <- cor(male_pts$a, male_pts$thalach, method = "pearson")
correlation_female <- cor(female_pts$a, female_pts$thalach, method = "pearson")
print(paste("Pearson correlation between Age and Max Heart Rate (Male):", correlation_male))
print(paste("Pearson correlation between Age and Max Heart Rate (Female):", correlation_female))

ggplot() +
  geom_point(data = male_pts, aes(x = thalach, y = a), alpha = 0.5, color = "blue") +
  geom_point(data = female_pts, aes(x = thalach, y = a), alpha = 0.5, color = "red") +
  geom_smooth(data = male_pts, aes(x = thalach, y = a), method = "lm", color = "blue", se = TRUE) +
  geom_smooth(data = female_pts, aes(x = thalach, y = a), method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Maximum Heart Rate (Male vs Female)",
       x = "Maximum Heart Rate", y = "Age") +
  theme_minimal()


############ F: oldpeak (reduced blood flow to heart) for Male and Female PTS
# Create a combined data frame for plotting
pts_data <- data.frame()

# Process the first 5 paths for males
for (i in 1:5) {
  path_indices <- raw_male_pts[[i]]
  target_values <- male_data$oldpeak[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Male", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Process the first 5 paths for females
for (i in 1:5) {
  path_indices <- raw_female_pts[[i]]
  target_values <- female_data$oldpeak[path_indices]
  
  temp_df <- data.frame(
    PseudoTime = 1:length(target_values),
    Stage = target_values,
    Path = factor(rep(i, length(target_values))),
    Gender = rep("Female", length(target_values))
  )
  
  pts_data <- rbind(pts_data, temp_df)
}

# Plot using ggplot2
library(ggplot2)
ggplot(pts_data, aes(x = PseudoTime, y = Stage, color = Gender)) +
  geom_line() +
  facet_wrap(~Path, scales = "free_y", ncol = 1) +
  labs(title = "Reduced Blood Flow to Heart (oldpeak) for First 5 Pseudo-Time Paths (Male vs Female)",
       x = "Pseudo-Time", y = "OldPeak(Reduced Blood Flow to Heart)") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()


#correlation between age and resting blood pressure 
correlation_male <- cor(male_pts$a, male_pts$oldpeak, method = "pearson")
correlation_female <- cor(female_pts$a, female_pts$oldpeak, method = "pearson")
print(paste("Pearson correlation between Age and Oldpeak - Reduced Blood Flow to Heart (Male):", correlation_male))
print(paste("Pearson correlation between Age and Oldpeak - Reduced Blood Flow to Heart (Female):", correlation_female))

ggplot() +
  geom_point(data = male_pts, aes(x = oldpeak, y = a), alpha = 0.5, color = "blue") +
  geom_point(data = female_pts, aes(x = oldpeak, y = a), alpha = 0.5, color = "red") +
  geom_smooth(data = male_pts, aes(x = oldpeak, y = a), method = "lm", color = "blue", se = TRUE) +
  geom_smooth(data = female_pts, aes(x = oldpeak, y = a), method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Age and Oldpeak - Reduced Blood Flow to Heart Rate (Male vs Female)",
       x = "Maximum Heart Rate", y = "Age") +
  theme_minimal()





