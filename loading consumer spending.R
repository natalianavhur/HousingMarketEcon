library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# Function to clean a single file
clean_file <- function(f, skip_lines = 13) {
  # Extract the year from the filename (e.g., "2013", "2014", etc.)
  year <- str_extract(basename(f), "20\\d{2}")
  
  # Read the file, assuming it has only 2 columns (without header names)
  df_raw <- read_excel(f, skip = skip_lines, col_names = FALSE)
  
  # Remove any columns that are entirely NA (if present)
  df_raw <- df_raw %>% select(where(~ !all(is.na(.))))
  
  # Rename the two columns: first as "item" and second as "value"
  df <- df_raw %>% rename(item = 1, value = 2)
  
  # Remove the header row if it exists (assuming it starts with "Item")
  if(tolower(df$item[1]) == "item") {
    df <- df[-1, ]
  }
  
  # Define the measure types that appear as separate rows
  measure_types <- c("Mean", "SE", "CV(%)")
  
  # Create a helper column that carries forward the actual item name
  # (i.e. rows that are not one of the measure types)
  df <- df %>%
    mutate(item_group = if_else(item %in% measure_types, NA_character_, item)) %>%
    fill(item_group, .direction = "down")
  
  # For rows that are measure rows, use the measure as given; for rows
  # that are not measure rows but do have a numeric value, assign a "Total" measure.
  df <- df %>%
    mutate(
      measure = if_else(item %in% measure_types, item, NA_character_),
      item = if_else(item %in% measure_types, item_group, item),
      measure = if_else(is.na(measure) & !is.na(value), "Total", measure)
    )
  
  # Remove rows that are just grouping headers (if both item and value are NA)
  df <- df %>% filter(!(is.na(item) & is.na(value)))
  
  # Add the year column
  df <- df %>% mutate(year = year)
  
  # Reorder columns to have: year, item, measure, and value
  df <- df %>% select(year, item, measure, value)
  
  df
}

# List all Excel files from 2013 to 2023 in the specified folder
consumer_files <- list.files(
  path = file.path("data", "macroeconomic", "consumer-spending"),
  pattern = "cu-all-detail-20.*\\.xlsx$",
  full.names = TRUE
)

# Process each file and combine the results into one tidy data frame
consumer_all_long <- map_dfr(consumer_files, clean_file, skip_lines = 13)

# Remove rows where item Aand value is NA
consumer_all_long <- consumer_all_long %>% filter(!(is.na(measure) & is.na(value)))

# Remove rows where item is NA
consumer_all_long <- consumer_all_long %>% filter(!is.na(item))


# Inspect the result
head(consumer_all_long)

