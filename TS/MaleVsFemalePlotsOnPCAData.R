# Install the dplyr package (if not already installed)
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")


# Load the dplyr package
library(dplyr)
library(ggplot2)


#read the data file
pca_data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\TS\\pca_transformed_time_series.csv", na.strings="?")
head(pca_data)

# Convert Stage and Sex to factors
pca_data$Stage <- as.factor(pca_data$Stage)
pca_data$Sex <- as.factor(pca_data$Sex)

# Separate data by gender

male_data <- pca_data %>% filter(Sex == 1)  # Male patients
nrow(male_data)
female_data <- pca_data %>% filter(Sex == 2)  # Female patients
nrow(female_data)

# Custom color mapping for disease stages
stage_colors <- c("0" = "blue", "1" = "green", "2" = "yellow", 
                  "3" = "orange", "4" = "red")

# PCA Scatter Plot for Males
plot_male <- ggplot(male_data, aes(x = PC1, y = PC2, color = Stage)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = stage_colors, 
                     name = "Disease Stage",
                     labels = c("0: Baseline", "1: Stage 1", 
                                "2: Stage 2", "3: Stage 3", 
                                "4: Death")) +
  labs(title = "PCA of Time Series Data (Males)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "right")

# PCA Scatter Plot for Females
plot_female <- ggplot(female_data, aes(x = PC1, y = PC2, color = Stage)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = stage_colors, 
                     name = "Disease Stage",
                     labels = c("0: Baseline", "1: Stage 1", 
                                "2: Stage 2", "3: Stage 3", 
                                "4: Death")) +
  labs(title = "PCA of Time Series Data (Females)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "right")

# Print the plots
print(plot_male)
print(plot_female)

#drawing density plots for males and females to see disease distribution by stage 
# Density Plot for Males
plot_male_density <- ggplot(male_data, aes(x = Age, fill = Stage)) +
  geom_density(alpha = 0.6) +  
  scale_fill_manual(values = stage_colors, 
                    name = "Disease Stage",
                    labels = c("0: Baseline", "1: Stage 1", 
                               "2: Stage 2", "3: Stage 3", 
                               "4: Death")) +
  labs(title = "Age Distribution by Disease Stage (Males)",
       x = "Age",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "right")

# Density Plot for Females
plot_female_density <- ggplot(female_data, aes(x = Age, fill = Stage)) +
  geom_density(alpha = 0.6) +  
  scale_fill_manual(values = stage_colors, 
                    name = "Disease Stage",
                    labels = c("0: Baseline", "1: Stage 1", 
                               "2: Stage 2", "3: Stage 3", 
                               "4: Death")) +
  labs(title = "Age Distribution by Disease Stage (Females)",
       x = "Age",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "right")

# Print the plots
print(plot_male_density)
print(plot_female_density)





