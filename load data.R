library(readxl)
library(dplyr)
library(here)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tibble)
library(tseries)
library(writexl)
library(tidyverse)





#########################################
#################### CONSUMER-SPENDING DATA
consumer_spending_dir <- file.path("data", "macroeconomic", "consumer-spending")

# List all files that match the pattern "cu-all-detail-YYYY.xlsx"
files <- list.files(consumer_spending_dir, pattern = "cu-all-detail-\\d{4}\\.xlsx$", full.names = TRUE)

print(files)

# Read and combine all files.
# For each file, extract the year from the file name, read the file,
# add a Year column, and then combine all rows into one data frame.
consumer_spending <- map_dfr(files, function(file) {
  # Extract the year from the file name using a regular expression.
  year <- str_extract(basename(file), "\\d{4}")
  
  # Read the spreadsheet.
  df <- read_excel(file)
  
  # Add a new column for the year (converted to integer)
  df <- df %>% mutate(Year = as.integer(year))
  
  return(df)
})

# Inspect consumer spending merged data.
head(consumer_spending)

#########################################
#################### INCOME-LIMITS DATA

# Set the directory where the spreadsheets are stored.
employment_path <- file.path("data", "macroeconomic", "employment", "cpsa2024.xlsx")

#########################################
#################### INTEREST-RATES DATA
employment_path <- file.path("data", "macroeconomic", "employment", "cpsa2024.xlsx")

#########################################
#################### MORTGAGES-RATES DATA
employment_path <- file.path("data", "macroeconomic", "employment", "cpsa2024.xlsx")



## GDP analysis FOR ONLY ONE SHEET.
##########################

#read a specific sheet of excel.
preview <- read_excel(gdp_path, sheet = "T10105-A", n_max = 15, col_names = FALSE)
print(preview)

# By default use the first excel file.
# Skip blank row in my excel file.
gdp_data <- read_excel(gdp_path, skip =2)

# Check first few rows.
head(gdp_data)

# Check structure.
str(gdp_data)

# Check summary statistics.
summary(gdp_data)

# Check column names.
colnames(gdp_data)

# Check missing values.
sum(is.na(gdp_data))
colSums(is.na(gdp_data))

# Check rows and columns.
dim(gdp_data)




