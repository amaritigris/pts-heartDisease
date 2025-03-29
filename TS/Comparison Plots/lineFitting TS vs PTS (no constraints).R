# Install and load required packages
install.packages("tidyverse")  # If not installed
library(ggplot2)
library(dplyr)
library(gridExtra)

# Read CSV files
pseudo_time_series <- read.csv("D:/CS Year 3/FYP/PTS code/TS/evaluation_pca/pca_generated_pts_noConstraints.csv")
real_time_series <- read.csv("D:/CS Year 3/FYP/PTS code/TS/evaluation_pca/extracted_time_series.csv")
head(real_time_series)
head(pseudo_time_series)

# Remove missing values (if any)
pseudo_time_series <- na.omit(pseudo_time_series)
real_time_series <- na.omit(real_time_series)

pseudo_time_series$PseudoTime <- max(pseudo_time_series$PseudoTime) - pseudo_time_series$PseudoTime

pseudo_time_series$Age <- max(pseudo_time_series$Age) - pseudo_time_series$Age


# Fit a linear model for PC1
real_model_pc1 <- lm(PC1 ~ Age, data = real_time_series)
pseudo_model_pc1 <- lm(PC1 ~ Age, data = pseudo_time_series)

# Fit a linear model for PC2
real_model_pc2 <- lm(PC2 ~ Age, data = real_time_series)
pseudo_model_pc2 <- lm(PC2 ~ Age, data = pseudo_time_series)


# Visualization using ggplot2 for PC1
real_ts_plot <- ggplot(real_time_series, aes(x = Age, y = PC1)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Real-Time Series PCA (PC1)", x = "Age", y = "PC1") +
  theme_minimal()

pts_plot <- ggplot(pseudo_time_series, aes(x = Age, y = PC1)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Pseudo-Time Series PCA (PC1)", x = "PseudoTime", y = "PC1") +
  theme_minimal() 


# Arrange the two plots side by side
grid.arrange(real_ts_plot, pts_plot, ncol = 2)

# Visualization using ggplot2 for PC2
real_ts_plot <- real_ts_plot <- ggplot(real_time_series, aes(x = Age, y = PC2)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Real-Time Series PCA (PC2)", x = "Age", y = "PC2") +
  theme_minimal()

pts_plot <- ggplot(pseudo_time_series, aes(x = Age, y = PC2)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Pseudo-Time Series PCA (PC2)", x = "PseudoTime", y = "PC2") +
  theme_minimal()

# Arrange the two plots side by side
grid.arrange(real_ts_plot, pts_plot, ncol = 2)

# Compare slopes and intercepts
cat("Real-Time Series PC1 Slope:", coef(real_model_pc1)[2], "\n")
cat("Pseudo-Time Series PC1 Slope:", coef(pseudo_model_pc1)[2], "\n")

cat("Real-Time Series PC1 Intercept:", coef(real_model_pc1)[1], "\n")
cat("Pseudo-Time Series PC1 Intercept:", coef(pseudo_model_pc1)[1], "\n")

cat("Real-Time Series PC1 R-squared:", summary(real_model_pc1)$r.squared, "\n")
cat("Pseudo-Time Series PC1 R-squared:", summary(pseudo_model_pc1)$r.squared, "\n")

# Repeat for PC2
cat("Real-Time Series PC2 Slope:", coef(real_model_pc2)[2], "\n")
cat("Pseudo-Time Series PC2 Slope:", coef(pseudo_model_pc2)[2], "\n")

cat("Real-Time Series PC2 Intercept:", coef(real_model_pc2)[1], "\n")
cat("Pseudo-Time Series PC2 Intercept:", coef(pseudo_model_pc2)[1], "\n")

cat("Real-Time Series PC2 R-squared:", summary(real_model_pc2)$r.squared, "\n")
cat("Pseudo-Time Series PC2 R-squared:", summary(pseudo_model_pc2)$r.squared, "\n")

