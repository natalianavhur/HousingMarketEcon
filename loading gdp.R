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
  tidy_data <- data_clean %>%
    pivot_longer(
      cols = -c(Line, Indicator, Code),
      names_to = "Date",
      values_to = "Value"
    )
  
  # Ensure the Date column is cleaned up
  tidy_data <- tidy_data %>%
    mutate(Date = str_trim(Date))  # Remove spaces
  
  # Now, filter the rows so that only valid date headers are kept.
  if (frequency == "Annual") {
    tidy_data <- tidy_data %>% filter(grepl("^[0-9]{4}$", Date))  # Keep YYYY format
  } else if (frequency == "Quarterly") {
    tidy_data <- tidy_data %>% filter(grepl("^[0-9]{4}Q[1-4]$", Date))  # Keep YYYYQX format
  }
  
  # Convert Date and Value columns appropriately
  tidy_data <- tidy_data %>%
    mutate(
      Date = case_when(
        frequency == "Annual" ~ as.character(Date),  # Store years as strings
        frequency == "Quarterly" ~ paste0(substr(Date, 1, 4), "-Q", substr(Date, 6, 6)),  # Store "YYYY-QX"
        TRUE ~ NA_character_
      ),
      Value = suppressWarnings(as.numeric(Value)),  # Convert Value to numeric
      Frequency = frequency,
      Sheet = sheet_name,
      TableCode = table_code
    )
  
  # Debugging: Print detected unique dates to verify correctness
  print(paste("Sheet:", sheet_name, "- Unique Dates:", paste(unique(tidy_data$Date)[1:10], collapse = ", ")))
  
  return(tidy_data)
}


# Get sheet names and filter out non-data sheets
all_sheets <- excel_sheets(gdp_path)
data_sheets <- all_sheets[all_sheets != "Contents"]

# Process all sheets safely
gdp_list <- map(data_sheets, ~ reshape_gdp(.x, skip_rows = 7))

# Remove NULL entries from gdp_list before binding
gdp_list <- compact(gdp_list)

# Combine into one dataframe
gdp_all <- bind_rows(gdp_list)

# Now you can filter for the type of data you need:
gdp_annual <- gdp_all %>% filter(Frequency == "Annual")
gdp_quarterly <- gdp_all %>% filter(Frequency == "Quarterly")
gdp_T10105   <- gdp_all %>% filter(TableCode == "T10105")

# Check the results
print(head(gdp_all))                                                                                           print(head(gdp_annual))
print(head(gdp_quarterly))
print(head(gdp_T10105))

# Choose an indicator to plot
selected_indicator <- "Gross domestic product"

# Filter for the selected indicator
gdp_filtered <- gdp_annual %>% 
  filter(Indicator == selected_indicator & !is.na(Value))

# Plot the filtered data
ggplot(gdp_filtered, aes(x = as.numeric(Date), y = Value)) +  
  geom_line(linewidth = 1, color = "blue") +  # Use a single color for clarity
  labs(title = paste("Annual GDP Indicator:", selected_indicator),
       x = "Year",
       y = "Value (in index or billions, as appropriate)") +
  theme_minimal()