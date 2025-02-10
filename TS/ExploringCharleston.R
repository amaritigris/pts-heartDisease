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
    )
  ) %>%
  group_by(STUDYNO) %>%
  arrange(Date) %>%   # <-- This ensures sorting by date within each patient
  fill(Cholesterol, Age, Pressure, Gender, .direction = "down")

time_series <- selected_data %>% 
  select(STUDYNO, Date, Phase, Age, Gender, Cholesterol, Pressure)

print(time_series, 100)  # Check the first 10 rows
summary(time_series)   # Get an overview


#now we want to split each patient into different dataframes
patient_list <- split(time_series, time_series$STUDYNO)

#plot cholstrol over time for patient 2
ggplot(patient_list[[2]], aes(x = Age, y = Cholesterol)) +
  geom_line() +
  geom_point() +
  labs(title = "Cholesterol Levels Over Time - Patient 2", x = "Age", y = "Cholesterol") +
  theme_minimal()

#now we want to check the correlation 
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





