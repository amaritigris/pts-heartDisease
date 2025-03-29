# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
time_series_data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\TS\\evaluation\\time_series_data.csv")

time_series_data$Date <- as.Date(time_series_data$Date, format="%Y-%m-%d")

# Remove rows with missing dates or ages
time_series_data <- time_series_data %>% filter(!is.na(Date) & !is.na(Age) & !is.na(Cholesterol))
print(time_series_data)

# Group by STUDYNO and arrange by Date, then convert to a list
time_series_list <- time_series_data %>%
  filter(!is.na(Age) & !is.na(Date)) %>%  # Remove missing values for Age and Date
  arrange(STUDYNO, Date) %>%  # Sort by STUDYNO and Date
  group_by(STUDYNO) %>%  # Group by STUDYNO
  group_split()  # Split the data into a list by STUDYNO

# Now 'time_series_list' is a list where each element corresponds to a STUDYNO's data
# For example, to view the first study's data:
head(time_series_list[[1]])

# Compute the average age per date
avg_age_time <- time_series_data %>%
  group_by(Date) %>%
  summarize(Average_Age = mean(Age, na.rm = TRUE))

# Plot the trend
ggplot(avg_age_time, aes(x = Date, y = Average_Age)) +
  geom_smooth(method = "lm", color = "blue", size = 1.2) +  # Linear trend
  labs(title = "Average Age Progression Over Time",
       x = "Time",
       y = "Average Age") +
  theme_minimal()

# Fit a linear model to get the rate of increase in age over time
rate_model <- lm(Average_Age ~ as.numeric(Date), data = avg_age_time)

# Extract and print the slope (rate of increase per time unit)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Age per Time Unit:", rate_of_increase, "\n")

# Compute the average cholesterol per date
avg_age_time <- time_series_data %>%
  group_by(Date) %>%
  summarize(Average_Age = mean(Cholesterol, na.rm = TRUE))

# Plot the trend
ggplot(avg_age_time, aes(x = Date, y = Average_Age)) +
  geom_smooth(method = "lm", color = "blue", size = 1.2) +  # Linear trend
  labs(title = "Average Cholesterol Progression Over Time",
       x = "Time",
       y = "Average Cholesterol") +
  theme_minimal()

# Fit a linear model to get the rate of increase in age over time
rate_model <- lm(Average_Age ~ as.numeric(Date), data = avg_age_time)

# Extract and print the slope (rate of increase per time unit)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Cholesterol per Time Unit:", rate_of_increase, "\n")

# Compute the average pressure per date
avg_age_time <- time_series_data %>%
  group_by(Date) %>%
  summarize(Average_Age = mean(Pressure, na.rm = TRUE))

# Plot the trend
ggplot(avg_age_time, aes(x = Date, y = Average_Age)) +
  geom_smooth(method = "lm", color = "blue", size = 1.2) +  # Linear trend
  labs(title = "Average Pressure Progression Over Time",
       x = "Time",
       y = "Average Pressure") +
  theme_minimal()

# Fit a linear model to get the rate of increase in age over time
rate_model <- lm(Average_Age ~ as.numeric(Date), data = avg_age_time)

# Extract and print the slope (rate of increase per time unit)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Pressure per Time Unit:", rate_of_increase, "\n")

# Compute the average Stage per date
avg_age_time <- time_series_data %>%
  group_by(Date) %>%
  summarize(Average_Age = mean(Stage, na.rm = TRUE))

# Plot the trend
ggplot(avg_age_time, aes(x = Date, y = Average_Age)) +
  geom_smooth(method = "lm", color = "blue", size = 1.2) +  # Linear trend
  labs(title = "Average Pressure Progression Over Time",
       x = "Time",
       y = "Average Pressure") +
  theme_minimal()

# Fit a linear model to get the rate of increase in age over time
rate_model <- lm(Average_Age ~ as.numeric(Date), data = avg_age_time)

# Extract and print the slope (rate of increase per time unit)
rate_of_increase <- coef(rate_model)[2]
cat("Rate of Increase in Pressure per Time Unit:", rate_of_increase, "\n")


