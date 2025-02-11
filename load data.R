library(readxl)
library(dplyr)
library(here)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tibble)


###############################################
################ GDP DATA
# Define the file path
gdp_path <- file.path("data", "macroeconomic", "gdp", "gdp_domestic_product_and_income.xlsx")

reshape_gdp <- function(sheet_name, skip_rows = 7) {
  # Determine frequency from sheet name:
  frequency <- ifelse(grepl("-A$", sheet_name), "Annual",
                      ifelse(grepl("-Q$", sheet_name), "Quarterly", NA))
  
  # Get the table code by removing the trailing "-A" or "-Q"
  table_code <- sub("-[AQ]$", "", sheet_name)
  
  # Read the sheet; skip the first 7 rows so that row 8 (the header) is read as data
  raw_data <- read_excel(gdp_path, sheet = sheet_name, skip = skip_rows, col_names = FALSE)
  
  # Extract the header row (row 8 in Excel becomes row 1 here)
  header <- as.character(raw_data[1, ])
  header[1] <- "Line"        
  header[2] <- "Indicator"   
  header[3] <- "Code"        
  colnames(raw_data) <- header
  
  # Remove the header row from the data
  data_clean <- raw_data[-1, ]
  
  # Pivot the data from wide to long format.
  # We assume that the first three columns are identifiers and the remaining columns are date values.
  tidy_data <- data_clean %>%
    pivot_longer(
      cols = -c(Line, Indicator, Code),
      names_to = "Date",
      values_to = "Value"
    )
  
  # Now, filter the rows so that only valid date headers are kept.
  # For annual data, we expect exactly four digits (e.g. "1929")
  # For quarterly data, we expect the header to start with a 4-digit year (e.g. "2000Q1" or "2000 Q1")
  if (frequency == "Annual") {
    tidy_data <- tidy_data %>% filter(grepl("^[0-9]{4}$", Date))
  } else if (frequency == "Quarterly") {
    tidy_data <- tidy_data %>% filter(grepl("^\\d{4}", Date))
  }
  
  # Convert the Date and Value columns to numeric.
  # We use suppressWarnings() here to hide conversion warnings;
  # however, since we already filtered out non-date columns, the warnings should be minimal.
  tidy_data <- tidy_data %>%
    mutate(
      Date = suppressWarnings(as.numeric(Date)),
      Value = suppressWarnings(as.numeric(Value)),
      Frequency = frequency,
      Sheet = sheet_name,
      TableCode = table_code
    )
  
  return(tidy_data)
}

# Get all sheet names and remove non-data sheets (like "Contents")
all_sheets <- excel_sheets(gdp_path)
data_sheets <- all_sheets[all_sheets != "Contents"]

# Load all sheets into a list
gdp_list <- map(data_sheets, ~ reshape_gdp(.x, skip_rows = 7))

# Combine all sheets into one data frame
gdp_all <- bind_rows(gdp_list)

# Now you can filter for the type of data you need:
gdp_annual <- gdp_all %>% filter(Frequency == "Annual")
gdp_quarterly <- gdp_all %>% filter(Frequency == "Quarterly")
gdp_T10105   <- gdp_all %>% filter(TableCode == "T10105")

# Check the results
print(head(gdp_all))
print(head(gdp_annual))
print(head(gdp_quarterly))
print(head(gdp_T10105))



ggplot(gdp_annual, aes(x = Date, y = Value, color = Indicator)) +
  geom_line(size = 1) +
  labs(title = "Annual GDP Indicators Over Time",
       x = "Year",
       y = "Value (in index or billions, as appropriate)",
       color = "Indicator") +
  theme_minimal()


ggplot(gdp_quarterly, aes(x = Date, y = Value, color = Indicator)) +
  geom_line(size = 1) +
  labs(title = "Quarterly GDP Indicators Over Time",
       x = "Year (Quarterly)",
       y = "Value",
       color = "Indicator") +
  theme_minimal()


ggplot(gdp_T10105, aes(x = Date, y = Value, color = Indicator)) +
  geom_line(size = 1) +
  labs(title = "GDP Data from Table T10105 (Annual)",
       x = "Year",
       y = "GDP Value",
       color = "Indicator") +
  theme_minimal()


#########################################
#################### INFLATION PRICES DATA
inflation_prices_path <- file.path("data", "macroeconomic", "inflation-prices", 
                                   "Consumer Price Index for All Urban Consumers (CPI-U).xlsx")

cpi_data <- read_excel(inflation_prices_path)

head(cpi_data)

cpi_data_long <- cpi_data %>%
  pivot_longer(
    cols = -Year,         # Keep the Year column intact.
    names_to = "Month",   # All other columns (e.g., Jan, Feb, …, HALF1, HALF2) become a new 'Month' column.
    values_to = "CPI"     # Their values will be stored in the 'CPI' column.
  )

head(cpi_data_long)

#########################################
#################### EMPLOYMENT DATA
employment_path <- file.path("data", "macroeconomic", "employment", "cpsa2024.xlsx")

# Define a function to load an employment sheet and clean it
read_employment_sheet <- function(file_path, sheet, skip_rows = 5) {
  
  df <- read_excel(file_path, sheet = sheet, skip = skip_rows, col_names = TRUE)
  
  # Convert every column to character and trim leading/trailing spaces.
  df_clean <- df %>%
    mutate(across(everything(), ~ as.character(.) %>% str_trim()))
  
  # Clean null and empty values.
  df_clean <- df_clean %>%
    select(where(~ !all(is.na(.)) && !all(. == "")))
  
  return(df_clean)
}


# Load a specific sheet.
emp_cpsaat01 <- read_employment_sheet(employment_path, sheet = "cpsaat01", skip_rows = 5)

# Inspect the cleaned data
print(head(emp_cpsaat01))

# Get all sheet names from the employment file
all_employment_sheets <- excel_sheets(employment_path)

for (sheet in all_employment_sheets) {
  cat("Sheet:", sheet, "\n")
  # Read the first 8 rows from the current sheet.
  preview <- read_excel(employment_path, sheet = sheet, n_max = 8)
  print(preview)
  cat("\n-----------------------\n\n")
}

# Read and clean all sheets (if they share the same header structure)
employment_list <- map(all_employment_sheets, ~ read_employment_sheet(employment_path, sheet = .x, skip_rows = 5))

# Optionally, add a column to track the source sheet and then combine:
employment_list <- imap(employment_list, ~ mutate(.x, Sheet = .y))
employment_all <- bind_rows(employment_list)

# Now 'employment_all' holds the combined data from all sheets.
head(employment_all)
str(employment_all)
print(employment_all)
View(employment_all)
colnames(employment_all)


# Create a data dictionary for your employment variables
employment_dict <- tribble(
  ~Variable,                                   ~Description,                                                              ~SourceSheet,  ~Unit,                ~Notes,
  "Year",                                      "Calendar year of observation",                                            "cpsaat01",    "Year",               "Common across all sheets",
  "Civilian_noninstitutional",                 "Civilian noninstitutional population (persons aged 16 and over)",         "cpsaat01",    "thousands",          "Overall population eligible for labor force participation",
  "LaborForce_Total",                          "Total number in the civilian labor force",                                "cpsaat01",    "thousands",          "Sum of employed and unemployed persons",
  "Civilian_Labor_Force_Total",                "Total number of individuals in the civilian labor force",                 "cpsaat01",    "thousands",          "Employed segment of the labor force (may be derived from CPS data)",
  "Civilian_Labor_Force",                      "Total number of employed persons",                                        "cpsaat01",    "thousands or %",     "Check documentation: may indicate either the count or the participation rate",
  "Employed_Total",                            "Alternative measure of the civilian labor force",                         "cpsaat01",    "thousands",          "Overall employment count for persons aged 16+",
  "Employed_Population",                       "Total number of employed persons",                                        "cpsaat01",    "thousands",          "Often similar to Employed_Total but may provide additional detail",
  "Employed_Agriculture",                      "Employed population; may include breakdown by full-time and part-time",   "cpsaat01",    "thousands",          "Subset of the employed population in agriculture",
  "Employed_Nonagri-cultural_Industries",      "Number of persons employed in the agriculture sector",                    "cpsaat01",    "thousands",          "Subset of the employed population in nonagricultural industries",
  "Employed_Percent",                          "Number of persons employed in nonagricultural industries",                "cpsaat01",    "thousands",          "Employment rate among the labor force",
  "Unemployed_Number",                         "Percentage of the civilian labor force that is employed",                 "cpsaat01",    "percentage",         "Aggregated unemployment numbers",
  "Unemployed_Labor_Force",                    "Total number of unemployed persons",                                      "cpsaat49",    "thousands",          "2024 annual averages",
  "No_Labor_Force",                            "Median weekly earnings of full-time wage and salary workers",             "cpsaat37",    "dollars",            "2024 annual averages; note: variable name may be misleading—consider renaming to 'FullTime_Median_Earnings'",
  "Men",                                       "Total number of individuals not in the labor force",                      "cpsaat01",    "thousands",          "Those not actively participating in the labor market (e.g., retirees, students)",
  
  
)

# Named vector that maps raw names to standardized names.
rename_mapping <- c(
  "...1" = "Year", 
  "...2" = "CivilianPopulation", 
  "...3" = "LaborForce_Total", 
  "Total" = "Employed_Total"
  
)

# Apply the renaming to your data frame.
emp_01 <- emp_01 %>%
  rename(!!!rename_mapping)

# View the data dictionary
print(employment_dict)

combined_data <- left_join(gdp_data, cpsaat01, by = "Year")


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




