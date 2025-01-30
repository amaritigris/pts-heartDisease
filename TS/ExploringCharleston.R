# Install packages if not already installed (run this once)
# Define required packages
required_packages <- c("readxl", "dplyr", "tidyr", "lubridate", "ggplot2", "xts")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)

# Load the libraries
lapply(required_packages, library, character.only = TRUE)

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

