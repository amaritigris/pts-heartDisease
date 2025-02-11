# Install the dplyr package (if not already installed)
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")


# Load the dplyr package
library(dplyr)
library(ggplot2)


#read the data file
data <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\TS\\pca_transformed_time_series.csv", na.strings="?")
head(data)

