# Install the dplyr package (if not already installed)
if (!require("dplyr")) install.packages("dplyr")

# Load the dplyr package
library(dplyr)


#read the data file
data <- read.table("D:\\CS Year 3\\FYP\\processed.cleveland.data", sep=",", header= FALSE, na.strings="?")

#assign column names based on UCI documentation 
colnames(data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", 
                    "ca", "thal", "target")

#view the first few rows
head(data)

#handle missing values
colSums(is.na(data))

#trying 2 things here and will go with what gives us better results
#option 1 - remove rows with missing values
data_clean <- na.omit(data)
data <- data_clean


#PERFORM PCA ----------------------------------------------------------- 

#prepate data for PCA
#select numerical features excuding target 
pca_features <- data %>% select(-target, -age)
#scale the data
pca_scaled <- scale(pca_features)
#perform PCA
pca_result <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)
#summary of PCA
summary(pca_result)
#extract the first 2 principal components 
pca_data <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_data) <- c("X","Y")

#add the target variable back to the PCA results
pca_data$Class <- data$target
# Add back the age column for use in PTS constraints
pca_data$Age <- data$age

#view the transformed data 
head(pca_data)

#save pca results to a csv file 
write.csv(pca_data, "pca_transformed_cleveland.csv", row.names = FALSE)

# Visualize PCA results with disease stages (original target values)
ggplot(pca_data, aes(x = X, y = Y, color = as.factor(Class))) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("0" = "blue", "1" = "green", 
                                "2" = "yellow", "3" = "orange", 
                                "4" = "red"),
                     name = "Disease Stage",
                     labels = c("0: No Disease", "1: Stage 1", 
                                "2: Stage 2", "3: Stage 3", 
                                "4: Stage 4")) +
  labs(title = "PCA of Cleveland Heart Disease Data by Disease Stage",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "right")
