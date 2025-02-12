# Install the dplyr package (if not already installed)
if (!require("dplyr")) install.packagesackages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")


# Load the dplyr package
library(dplyr)
library(ggplot2)


#read the data file
data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\TS\\time_series_data.csv", na.strings="?")
head(data)

data <- data %>%
  mutate(
    Age = as.numeric(Age), 
    Stage = as.numeric(Stage),
    Cholesterol = as.numeric(Cholesterol),
    Pressure = as.numeric(Pressure)
  )

data_clean <- na.omit(data)
head(data_clean)

# Select numerical features for PCA (excluding categorical variables)
pca_features <- data_clean %>%
  select(-STUDYNO, -Date, -Phase, -Gender)  # Exclude categorical and identifier columns

# Scale the numerical data
pca_scaled <- scale(pca_features)


# Perform PCA
pca_result <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Extract the first 2 principal components
pca_data <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_data) <- c("PC1", "PC2")

# Add the disease stage back to PCA results
pca_data$Stage <- data_clean$Stage
pca_data$Age <- data_clean$Age  # Add Age for reference
pca_data$Sex <- data_clean$Gender
pca_data$STUDYNO <- data_clean$STUDYNO  

# Convert Gender: 1 & 3 → Male (1), 2 & 4 → Female (2)
pca_data$Sex <- ifelse(data_clean$Gender %in% c(1, 3), 1, 2)  


# View the transformed data
head(pca_data)

# Save PCA results to a CSV file
write.csv(pca_data, "pca_transformed_time_series.csv", row.names = FALSE)

# 🔹 PCA Visualization with Disease Stages
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Stage))) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("0" = "blue", "1" = "green", 
                                "2" = "yellow", "3" = "orange", 
                                "4" = "red"),
                     name = "Disease Stage",
                     labels = c("0: Baseline", "1: Stage 1", 
                                "2: Stage 2", "3: Stage 3", 
                                "4: Death")) +
  labs(title = "PCA of Time Series Data by Disease Stage",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "right")
