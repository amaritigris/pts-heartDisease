# Load necessary libraries
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("xts")) install.packages("xts")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(xts)

#----------------------------STEP 1: Load all six Excel files and merge them into one dataset.
#read files in a loop and define file names
folder_path <- "D:\\CS Year 3\\FYP\\Charleston Data\\"
file_names = paste0(folder_path, "DS000", 1:6, ".xlsx")
print(file_names)

#read all files into a list 
data_list <- lapply(file_names, read_excel)
print(data_list)
#Assign phase names
phase_names <- c("Baseline", "Phase3", "Phase10", "Phase11", "Phase12", "Deaths")


# Add phase column to each dataset
for (i in seq_along(data_list)) {
  data_list[[i]]$Phase <- phase_names[i]
}

#combine all datasets into one
full_data <- bind_rows(data_list)
head(full_data)

#-------------------------------STEP 2: Assign time points based on study phases.

#ensure that full_data is a dataframe
full_data <- as.data.frame(full_data)
head(full_data)

#convert time variables into a standard date format 
# Convert time variables into a standard date format 
# Convert time variables into a standard date format 
library(dplyr)

# Convert time variables into a standard date format 
full_data <- full_data %>% 
  mutate(
    # Convert columns safely, replacing invalid entries with NA
    EXAMYY = as.numeric(EXAMYY),
    EXAMMM = as.numeric(EXAMMM),
    EXAMYY3 = as.numeric(EXAMYY3),
    EXAMMM3 = as.numeric(EXAMMM3),
    YEAR10 = as.numeric(YEAR10),
    MONTH10 = as.numeric(MONTH10),
    YEAR11 = as.numeric(YEAR11),
    MONTH11 = as.numeric(MONTH11),
    YEAR12 = as.numeric(YEAR12),
    CENS_YR = as.numeric(CENS_YR),
    DEATHMO = as.numeric(DEATHMO),
    
    # Ensure valid values (months should be 1-12, years >1900)
    EXAMMM = ifelse(EXAMMM %in% 1:12, EXAMMM, NA),
    EXAMMM3 = ifelse(EXAMMM3 %in% 1:12, EXAMMM3, NA),
    MONTH10 = ifelse(MONTH10 %in% 1:12, MONTH10, NA),
    MONTH11 = ifelse(MONTH11 %in% 1:12, MONTH11, NA),
    DEATHMO = ifelse(DEATHMO %in% 1:12, DEATHMO, NA),
    
    EXAMYY = ifelse(EXAMYY >= 1900 & EXAMYY <= 2100, EXAMYY, NA),
    EXAMYY3 = ifelse(EXAMYY3 >= 1900 & EXAMYY3 <= 2100, EXAMYY3, NA),
    YEAR10 = ifelse(YEAR10 >= 1900 & YEAR10 <= 2100, YEAR10, NA),
    YEAR11 = ifelse(YEAR11 >= 1900 & YEAR11 <= 2100, YEAR11, NA),
    YEAR12 = ifelse(YEAR12 >= 1900 & YEAR12 <= 2100, YEAR12, NA),
    CENS_YR = ifelse(CENS_YR >= 1900 & CENS_YR <= 2100, CENS_YR, NA),
    
    # Construct the date string BEFORE applying as.Date()
    DateString = case_when(
      Phase == "Baseline" & !is.na(EXAMYY) ~ sprintf("%d-%02d-01", EXAMYY, ifelse(is.na(EXAMMM), 6, EXAMMM)),
      Phase == "Phase3" & !is.na(EXAMYY3) ~ sprintf("%d-%02d-01", EXAMYY3, ifelse(is.na(EXAMMM3), 6, EXAMMM3)),
      Phase == "Phase10" & !is.na(YEAR10) ~ sprintf("%d-%02d-01", YEAR10, ifelse(is.na(MONTH10), 6, MONTH10)),
      Phase == "Phase11" & !is.na(YEAR11) ~ sprintf("%d-%02d-01", YEAR11, ifelse(is.na(MONTH11), 6, MONTH11)),
      Phase == "Phase12" & !is.na(YEAR12) ~ sprintf("%d-06-01", YEAR12),  # Default to June
      Phase == "Deaths" & !is.na(CENS_YR) ~ sprintf("%d-%02d-01", CENS_YR, ifelse(is.na(DEATHMO), 6, DEATHMO)),
      TRUE ~ NA_character_  # Ensure it's character, not factor
    )
  )

# Print some sample values
print(head(full_data$DateString, 20))  # See if any NA or malformed values exist

# If all looks good, convert DateString to Date format
full_data <- full_data %>%
  mutate(Date = as.Date(DateString, format="%Y-%m-%d"))

# Check summary again
summary(full_data$Date)
head(full_data %>% select(STUDYNO, Phase, Date))

#------------------------------Fill missing months with June (mid-year assumption)  
# Handle the date creation only for rows with valid EXAMYY values
full_data$Date[is.na(full_data$Date) & !is.na(full_data$EXAMYY)] <- 
  as.Date(paste(full_data$EXAMYY[!is.na(full_data$EXAMYY)], "06", "01", sep = "-"))

# Handle the date creation only for rows with valid EXAMYY3 values
full_data$Date[is.na(full_data$Date) & !is.na(full_data$EXAMYY3)] <- 
  as.Date(paste(full_data$EXAMYY3[!is.na(full_data$EXAMYY3)], "06", "01", sep = "-"))

# Handle the date creation only for rows with valid YEAR10 values
full_data$Date[is.na(full_data$Date) & !is.na(full_data$YEAR10)] <- 
  as.Date(paste(full_data$YEAR10[!is.na(full_data$YEAR10)], "06", "01", sep = "-"))

# Handle the date creation only for rows with valid YEAR11 values
full_data$Date[is.na(full_data$Date) & !is.na(full_data$YEAR11)] <- 
  as.Date(paste(full_data$YEAR11[!is.na(full_data$YEAR11)], "06", "01", sep = "-"))

# Handle the date creation only for rows with valid YEAR12 values
full_data$Date[is.na(full_data$Date) & !is.na(full_data$YEAR12)] <- 
  as.Date(paste(full_data$YEAR12[!is.na(full_data$YEAR12)], "06", "01", sep = "-"))

# Handle the date creation only for rows with valid DEATHYR values
full_data$Date[is.na(full_data$Date) & !is.na(full_data$DEATHYR)] <- 
  as.Date(paste(full_data$DEATHYR[!is.na(full_data$DEATHYR)], "06", "01", sep = "-"))

# Check summary again
summary(full_data$Date)
head(full_data %>% select(STUDYNO, Phase, Date))

#------------------------SORT DATA BY INDIVIDUAL AND DATE
full_data <- full_data %>% 
  arrange(STUDYNO,Date)
head(full_data$STUDYNO, 50)


#convert into time series format 


#-----------first i want to create a new dataframe for testing purposes

# Combine cholesterol values from different phases into one column

selected_data <- full_data %>%
  mutate(
    Cholesterol = case_when(
      Phase == "Baseline" ~ CHOL,
      Phase == "Phase11"  ~ CHOL11,
      Phase == "Phase12"  ~ CHOL12,
      Phase == "Additional" ~ SERCHOL3,
      TRUE ~ NA_real_
    ),
    Age = case_when(
      Phase == "Baseline" ~ AGE2,
      Phase == "Phase10"  ~ AGE10,
      Phase == "Phase11"  ~ AGE11,
      Phase == "Phase12"  ~ AGE12,
      Phase == "Additional" ~ AGE3,
      Phase == "Deaths" ~ CENS_YR - BIRTHYR,
      TRUE ~ NA_real_
    ),
    Pressure = case_when(
      Phase == "Baseline" ~ SBPPOST,
      Phase == "Phase10"  ~ SBP110,
      Phase == "Phase11"  ~ SBP111,
      Phase == "Phase12"  ~ SBP112,
      Phase == "Additional" ~ SYSSEAT3,
      TRUE ~ NA_real_
    ), 
    Gender = case_when(
      Phase == "Baseline" ~ RSEX,
      Phase == "Phase10"  ~ RSEX10,
      Phase == "Phase11"  ~ RSEX11,
      Phase == "Phase12"  ~ RSEX12,
      Phase == "Additional" ~ RSEX3,
      TRUE ~ NA_real_
    ),
    Stage = case_when(
      Phase == "Baseline" ~ 0, 
      Phase == "Phase10"  ~ 1 ,
      Phase == "Phase11"  ~ 2,
      Phase == "Phase12" ~ 3,
      Phase == "Deaths" ~ 4, 
      Phase == "Additional" ~ 0
    )
  ) %>%
  group_by(STUDYNO) %>%
  arrange(Date) %>%   # <-- This ensures sorting by date within each patient
  fill(Cholesterol, Age, Pressure, Gender, Stage, .direction = "down")

time_series <- selected_data %>% 
  select(STUDYNO, Date, Phase, Age, Gender, Stage, Cholesterol, Pressure)

print(time_series, 100)  # Check the first 10 rows
summary(time_series)   # Get an overview

#now we want to split each patient into different dataframes
patient_list <- split(time_series, time_series$STUDYNO)
patient_list[[1]]

#calculate average age by stage
avg_age_stage <- selected_data %>%
  group_by(Stage) %>%  # Group by Stage
  summarise(Avg_Age = mean(Age, na.rm = TRUE)) %>%
  arrange(Stage)  # Ensure chronological order

ggplot(avg_age_stage, aes(x = Stage, y = Avg_Age)) +
  geom_line(color = "green", size = 1) +  # Line plot for trend
  geom_point(color = "darkred", size = 2) +  # Points for individual averages
  scale_x_continuous(breaks = 0:4, labels = c("0", "1", "2", "3", "4")) +  
  labs(title = "Average Age by Disease Stage",
       x = "Stage",
       y = "Average Age") +
  theme_minimal()

#finding the correlation between age and disease stage
# Compute Spearman correlation between Age and Stage
cor_age_stage <- cor(selected_data$Age, selected_data$Stage, method = "spearman", use = "complete.obs")

# Print the correlation result
print(paste("Spearman Correlation between Age and Stage: ", cor_age_stage))

# Scatter plot for Age vs Stage
ggplot(selected_data, aes(x = Age, y = Stage)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points showing the relationship
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add trend line
  labs(title = "Correlation Between Age and Disease Stage",
       x = "Age",
       y = "Disease Stage (0=Baseline, 1=Phase10, 2=Phase11, 3=Phase12, 4=Deaths)") +
  theme_minimal() +
  theme(legend.position = "none")


#calculate average cholestrol by age 
avg_cholesterol <- selected_data %>%
  group_by(Age) %>%  # Group by Age
  summarise(Avg_Cholesterol = mean(Cholesterol, na.rm = TRUE)) %>%  # Compute mean cholesterol
  arrange(Age)  # Ensure chronological order

# Plot the time series for average cholesterol
ggplot(avg_cholesterol, aes(x = Age, y = Avg_Cholesterol)) +
  geom_line(color = "blue", size = 1) +  # Line plot for trend
  geom_point(color = "red", size = 2) +  # Points for individual averages
  labs(title = "Average Cholesterol Levels Over Time",
       x = "Age",
       y = "Average Cholesterol") +
  theme_minimal()

#calculate average cholestrol by stage
avg_cholesterol_stage <- selected_data %>%
  group_by(Stage) %>%  # Group by Stage
  summarise(Avg_Cholesterol = mean(Cholesterol, na.rm = TRUE)) %>%
  arrange(Stage)  # Ensure chronological order

ggplot(avg_cholesterol_stage, aes(x = Stage, y = Avg_Cholesterol)) +
  geom_line(color = "blue", size = 1) +  # Line plot for trend
  geom_point(color = "red", size = 2) +  # Points for individual averages
  scale_x_continuous(breaks = 0:4, labels = c("0", "1", "2", "3", "4")) +  
  labs(title = "Average Cholesterol Levels by Disease Stage",
       x = "Stage",
       y = "Average Cholesterol") +
  theme_minimal()

#now we want to check the correlation between cholesterol and age 
clean_data <- time_series[!is.na(time_series$Age) & !is.na(time_series$Cholesterol), ]
# Scatter plot with regression line
ggplot(clean_data, aes(x = Age, y = Cholesterol)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(title = "Correlation Between Age and Cholesterol",
       x = "Age (years)", 
       y = "Cholesterol Levels") +
  theme_minimal()
cor(clean_data$Age, clean_data$Cholesterol, use = "complete.obs")


#check correlation between cholestrol and stage
clean_data <- time_series[!is.na(time_series$Stage) & !is.na(time_series$Cholesterol), ]
# Scatter plot with regression line
ggplot(clean_data, aes(x = Stage, y = Cholesterol)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(title = "Correlation Between Stage and Cholesterol",
       x = "Age (years)", 
       y = "Cholesterol Levels") +
  theme_minimal()
cor(clean_data$Stage, clean_data$Cholesterol, use = "complete.obs")

#calculate average pressure by age
avg_pressure <- selected_data %>%
  group_by(Age) %>%  # Group by Age
  summarise(Avg_Pressure = mean(Pressure, na.rm = TRUE)) %>%  # Compute mean pressure
  arrange(Age)  # Ensure chronological order

# Plot the time series for average pressure
ggplot(avg_pressure, aes(x = Age, y = Avg_Pressure)) +
  geom_line(color = "blue", size = 1) +  # Line plot for trend
  geom_point(color = "red", size = 2) +  # Points for individual averages
  labs(title = "Average Pressure Levels Over Time",
       x = "Age",
       y = "Average Pressure") +
  theme_minimal()

#calculate average pressure by stage
avg_pressure <- selected_data %>%
  group_by(Stage) %>%  # Group by Stage
  summarise(Avg_Pressure = mean(Pressure, na.rm = TRUE)) %>%  # Compute mean pressure
  arrange(Stage)  # Ensure chronological order

# Plot the time series for average pressure
ggplot(avg_pressure, aes(x = Stage, y = Avg_Pressure)) +
  geom_line(color = "blue", size = 1) +  # Line plot for trend
  geom_point(color = "red", size = 2) +  # Points for individual averages
  labs(title = "Average Pressure Levels Over Disease Stages",
       x = "Stage",
       y = "Average Pressure") +
  theme_minimal()


#now we want to check the correlation between pressure and age 
clean_data <- time_series[!is.na(time_series$Age) & !is.na(time_series$Pressure), ]
# Scatter plot with regression line
ggplot(clean_data, aes(x = Age, y = Pressure)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(title = "Correlation Between Age and Pressure",
       x = "Age (years)", 
       y = "Pressure Levels") +
  theme_minimal()
cor(clean_data$Age, clean_data$Pressure, use = "complete.obs")

#now we want to check the correlation between pressure and stage
clean_data <- time_series[!is.na(time_series$Stage) & !is.na(time_series$Pressure), ]
# Scatter plot with regression line
ggplot(clean_data, aes(x = Stage, y = Pressure)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(title = "Correlation Between Stage and Pressure",
       x = "Disease Stage", 
       y = "Pressure Levels") +
  theme_minimal()
cor(clean_data$Stage, clean_data$Pressure, use = "complete.obs")


#creating a density plot to see how different age groups are distributed within different disease stages
# Density plot of Age across Disease Stages
ggplot(selected_data, aes(x = Age, fill = factor(Stage))) +
  geom_density(alpha = 0.6) +  # Fill the area under the curve for better visualization
  scale_fill_brewer(palette = "Set1", name = "Disease Stage") +  # Color the stages differently
  labs(title = "Density Plot of Age Across Disease Stages",
       x = "Age",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top")  # Position legend at the top





