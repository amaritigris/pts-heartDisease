# Install the dplyr package (if not already installed)
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")


# Load the dplyr package
library(dplyr)
library(ggplot2)


#read the data file
pca_data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\TS\\pca_transformed_time_series.csv", na.strings="?")
head(pca_data)

# Convert Stage to factor for better visualization
pca_data$Stage <- as.factor(pca_data$Stage)

#select 300 unique patients 
set.seed(123) #ensure reproducibility
selected_patients <- sample(unique(pca_data$STUDYNO), 100)

# Filter dataset to only include the selected patients
pca_sampled <- pca_data %>% filter(STUDYNO %in% selected_patients)


# Plot disease progression for each selected patient (time series)
ggplot(pca_sampled, aes(x = PC1, y = PC2, color = Stage, group = STUDYNO)) +
  geom_point(size = 3, alpha = 0.8) +  # Scatter points
  geom_line(colour = "black", alpha = 0.5) +  # Connect points within the same patient
   scale_color_manual(values = c("0" = "blue", "1" = "green", 
                                "2" = "yellow", "3" = "orange", 
                                "4" = "red"),
                     name = "Disease Stage",
                     labels = c("0: No Disease", "1: Stage 1", 
                                "2: Stage 2", "3: Stage 3", 
                                "4: Stage 4")) +
  labs(title = "Disease Progression Trajectories (Sampled Patients)",
       x = "Principal Component 1 (PC1)",
       y = "Principal Component 2 (PC2)") +
  theme_minimal() +
  theme(legend.position = "right")


#plot to show age distribution of patient data and where they are on the disease scale 
# Convert Age to numeric (if it's still a character)
pca_sampled$Age <- as.numeric(pca_sampled$Age)

# Define Age Groups for better visualization
pca_sampled$Age_Group <- cut(pca_sampled$Age, 
                          breaks = c(0, 30, 40, 50, 60, 70, 80, 100), 
                          labels = c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                          include.lowest = TRUE)

# Scatter plot for Age vs Disease Stage
ggplot(pca_sampled, aes(x = Age, y = Stage, color = Age_Group)) +
  geom_jitter(width = 0.3, height = 0.1, alpha = 0.7, size = 3) +  # Jitter to reduce overlap
  scale_color_manual(values = c("<30" = "purple", "30-39" = "blue", 
                                "40-49" = "green", "50-59" = "pink", 
                                "60-69" = "orange", "70-79" = "red", 
                                "80+" = "brown")) +
  labs(title = "Age Group Distribution Across Disease Stages",
       x = "Age",
       y = "Disease Stage",
       color = "Age Group") +
  theme_minimal() +
  theme(legend.position = "right")

#saving time series for comparison purposes
# Select relevant columns and order by STUDYNO and time (if a time column exists)
ts_data <- pca_sampled %>% 
  select(STUDYNO, Age, Stage, PC1, PC2) %>%
  arrange(STUDYNO, Age)  # Ensuring proper ordering of time series

# Print first few rows to verify structure
head(ts_data)

write.csv(ts_data, "D:\\CS Year 3\\FYP\\PTS code\\TS\\extracted_time_series.csv", row.names = FALSE)



