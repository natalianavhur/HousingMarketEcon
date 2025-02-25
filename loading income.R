library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

process_cex_file <- function(file_path) {
  
  # 1. Read once to find which row has "Item" in the first column
  raw_data <- read_excel(file_path, col_names = FALSE)
  
  header_row <- which(raw_data[[1]] == "Item")
  if (length(header_row) == 0) {
    warning(paste("No 'Item' row found in:", basename(file_path)))
    return(NULL)
  }
  
  # We'll skip everything above that row so that row becomes our column headers
  skip_n <- header_row - 1
  
  # 2. Read again, but force all columns to text
  df <- read_excel(
    file_path,
    skip = skip_n,
    col_names = TRUE,
    col_types = "text"  # <— Force all columns as text
  )
  
  # 3. Clean up the column names
  # For example, remove line breaks, quotes, extra spaces, etc.
  old_names <- names(df)
  new_names <- old_names %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_replace_all('"', "") %>%
    str_replace_all("\\s{2,}", " ")
  
  names(df) <- new_names
  
  # 4. Rename columns systematically if you want
  # (Adjust these mappings as needed for your actual column headers)
  rename_map <- c(
    "Aggregate"            = "Aggregate",
    "Lowest 10 percent"    = "decile_1",
    "Second 10 percent"    = "decile_2",
    "Third 10 percent"     = "decile_3",
    "Fourth 10 percent"    = "decile_4",
    "Fifth 10 percent"     = "decile_5",
    "Sixth 10 percent"     = "decile_6",
    "Seventh 10 percent"   = "decile_7",
    "Eighth 10 percent"    = "decile_8",
    "Ninth 10 percent"     = "decile_9",
    "Highest 10 percent"   = "decile_10"
  )
  
  for (col_old in names(rename_map)) {
    if (col_old %in% names(df)) {
      col_new <- rename_map[[col_old]]
      df <- df %>% rename(!!col_new := !!sym(col_old))
    }
  }
  
  # 5. Add a 'year' column extracted from the filename
  year <- str_extract(basename(file_path), "20\\d{2}")
  df <- df %>% mutate(year = year)
  
  df <- df %>%
    # Remove rows where *every* column is NA or an empty string
    filter(
      !if_all(everything(), ~ is.na(.x) | .x == "")
    )
  
  # 6. Filter out footnotes or disclaimers (adjust these as needed)
  df <- df %>%
    filter(
      !str_detect(Item, "^Source:") & 
        !str_detect(Item, "^a/") &
        !str_detect(Item, "^b/") &
        !str_detect(Item, "^c/") &
        !is.na(Item)
    )
  
  # 7. Pivot decile columns longer into (decile, value)
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("decile_"),
      names_to = "decile",
      values_to = "value"
    )
  
  # 8. Clean, convert "value" to numeric, and remove rows where conversion fails
  df_long <- df_long %>%
    mutate(
      cleaned_value = str_remove_all(value, "[$,]") %>% 
        str_replace_all("c/", "") %>%
        str_trim(),
      numeric_value = suppressWarnings(as.numeric(cleaned_value))
    ) %>%
    # Remove rows where numeric conversion resulted in NA
    filter(!is.na(numeric_value)) %>%
    # Overwrite the original value with the numeric conversion
    mutate(value = numeric_value) %>%
    select(-cleaned_value, -numeric_value)
  
  return(df_long)
}

# Then map over all your files and bind them
income_files <- list.files(
  path = file.path("data", "macroeconomic", "income-limits", "Deciles of income before taxes"),
  pattern = "cu-income-deciles-before-taxes-20(14|15|16|17|18|19|20|21|22|23)\\.xlsx$",
  full.names = TRUE
)

income_data <- map_dfr(income_files, process_cex_file)

# Now `income_data` is a combined, tidy dataset with columns:
#   Item, decile, value, year, etc.



