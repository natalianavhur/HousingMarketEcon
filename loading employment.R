library(readxl)
library(dplyr)
library(zoo)
library(openxlsx)
library(tidyr)
library(purrr)
library(stringr)

# ------------------------------------------------------------------
# 1) Set up your input/output files
# ------------------------------------------------------------------
input_file <- file.path("data", "macroeconomic", "employment", "cpsa2024.xlsx")
output_file <- "cpsa2024_cleaned.xlsx"

# Read all sheet names in the workbook
sheet_names <- excel_sheets(input_file)

# This will hold the cleaned data frames for each sheet
cleaned_sheets <- list()

# ------------------------------------------------------------------
# 2) Define a helper function to detect and clean one sheet
# ------------------------------------------------------------------
clean_one_sheet <- function(sheet_name) {
  
  # Read entire sheet with no column names
  raw <- read_excel(input_file, sheet = sheet_name, col_names = FALSE)
  
  # ----------------------------------------------------------------
  # Skipping rows 1-2 (based on your note)
  # But keep them in 'raw' for indexing. We'll just ignore them later.
  # ----------------------------------------------------------------
  
  # We suspect the multi-row headers are in rows 4–6 or 4–7.
  # Let's check if row 7 is empty (all NA). If so, assume headers are rows 4–6.
  # Otherwise, assume headers are rows 4–7.
  # (You can adapt this logic if your file structure differs.)
  if (nrow(raw) >= 7 && all(is.na(raw[7, ]))) {
    header_rows <- 4:6
    # If row 7 is empty, data starts at row 8 (after the empty row)
    data_start <- 8
  } else {
    header_rows <- 4:7
    # Then data would start at row 9
    data_start <- 9
  }
  
  # ----------------------------------------------------------------
  # 3) Extract the header rows and "forward-fill" horizontally
  #    to un-merge cells
  # ----------------------------------------------------------------
  header_data <- raw[header_rows, ] %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  # Forward-fill each row horizontally
  header_filled <- header_data %>%
    t() %>%
    apply(., 2, function(x) na.locf(x, na.rm = FALSE)) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  # ----------------------------------------------------------------
  # 4) Flatten the multi-row headers into a single header line
  # ----------------------------------------------------------------
  new_header <- apply(header_filled, 2, function(col_vals) {
    # Remove actual NA values
    col_vals <- col_vals[!is.na(col_vals)]
    # Also remove any literal "NA" strings
    col_vals <- col_vals[col_vals != "NA"]
    
    # Concatenate the remaining text with underscores
    merged <- paste(col_vals, collapse = "_")
    
    # Remove line breaks
    merged <- gsub("[\r\n]+", " ", merged)
    
    # Replace multiple underscores with a single underscore
    merged <- gsub("_+", "_", merged)
    
    # Remove leading/trailing underscores
    merged <- gsub("^_|_$", "", merged)
    
    # Clean extra spaces
    merged <- gsub("\\s+", " ", merged)
    merged <- trimws(merged)
    
    merged
  })
  
  # ----------------------------------------------------------------
  # 5) Create the final data portion
  #    Remove rows 1–2 (always skipped), the header rows, and the
  #    single empty row after them. Data starts at 'data_start'.
  # ----------------------------------------------------------------
  # So the rows to remove are: 1, 2, plus 'header_rows', plus the empty row (data_start - 1).
  rows_to_remove <- c(1, 2, header_rows, data_start - 1)
  
  # Make sure those rows exist in the data (in case of a small sheet, etc.)
  rows_to_remove <- rows_to_remove[rows_to_remove <= nrow(raw)]
  
  data_clean <- raw[-rows_to_remove, ]
  
  # Assign the single-row header to the data
  colnames(data_clean) <- new_header
  
  # Return the cleaned data frame
  data_clean
}

# ------------------------------------------------------------------
# 6) Loop over each sheet, clean it, store in a list
# ------------------------------------------------------------------
for (sh in sheet_names) {
  cat("Cleaning sheet:", sh, "...\n")
  cleaned_sheets[[sh]] <- clean_one_sheet(sh)
}

# ------------------------------------------------------------------
# 7) Write all cleaned sheets into one Excel file
# ------------------------------------------------------------------
wb <- createWorkbook()
for (sh in sheet_names) {
  addWorksheet(wb, sheetName = sh)
  writeData(wb, sheet = sh, cleaned_sheets[[sh]])
}
saveWorkbook(wb, output_file, overwrite = TRUE)
cat("All sheets cleaned and saved to", output_file, "\n")





library(readxl)



library(readxl)
library(dplyr)
library(tidyr)

# Read the Excel sheet
cpsaat01 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat01")

# Pivot the data into long format
cpsaat01_long <- cpsaat01 %>%
  pivot_longer(
    cols = c(
      "Civilian labor force_Employed_Percent of population",
      "Civilian labor force_Employed_Agri- culture",
      "Civilian labor force_Employed_Nonagri- cultural industries",
      "Civilian labor force_Unemployed_Number",
      "Civilian labor force_Unemployed_Percent of labor force",
      "Not in labor force_Unemployed_Percent of labor force"
    ),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(
    Parameter = case_when(
      Parameter == "Civilian labor force_Employed_Percent of population" ~ "employed population ratio",
      Parameter == "Civilian labor force_Employed_Agri- culture" ~ "employed agri",
      Parameter == "Civilian labor force_Employed_Nonagri- cultural industries" ~ "employed nonagri",
      Parameter == "Civilian labor force_Unemployed_Number" ~ "unemployed",
      Parameter == "Civilian labor force_Unemployed_Percent of labor force" ~ "unemployed rate",
      Parameter == "Not in labor force_Unemployed_Percent of labor force" ~ "not in labor force",
      TRUE ~ Parameter  # default case if any unexpected column appears
    ),
    # Add extra fields as required; adjust these values if needed
    Ethnicity = "All",
    Gender = "All",
    Age = "16 years and over",
    Sheet = "cpsaat01"
  ) %>%
  # Ensure the column order matches your requirements
  select(Year, Ethnicity, Gender, Age, Parameter, Value, Sheet)


cpsaat01_long <- cpsaat01_long %>%
  filter(grepl("^[0-9]+$", Year))


cpsaat02 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat02")

cpsaat02_long <- cpsaat02_clean %>%
  pivot_longer(
    cols = c(
      "Civilian noninsti- tutional population",
      "Civilian labor force_Total",
      "Civilian labor force_Percent of population",
      "Civilian labor force_Employed_Total",
      "Civilian labor force_Employed_Percent of population",
      "Civilian labor force_Employed_Agri- culture",
      "Civilian labor force_Employed_Nonagri- cultural industries",
      "Civilian labor force_Unemployed_Number",
      "Civilian labor force_Unemployed_Percent of labor force",
      "Not in labor force_Unemployed_Percent of labor force"
    ),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(
    Parameter = case_when(
      Parameter == "Civilian noninsti- tutional population" ~ "population",
      Parameter == "Civilian labor force_Total" ~ "labor force",
      Parameter == "Civilian labor force_Percent of population" ~ "labor force participation rate",
      Parameter == "Civilian labor force_Employed_Total" ~ "employed",
      Parameter == "Civilian labor force_Employed_Percent of population" ~ "employed population ratio",
      Parameter == "Civilian labor force_Employed_Agri- culture" ~ "employed agri",
      Parameter == "Civilian labor force_Employed_Nonagri- cultural industries" ~ "employed nonagri",
      Parameter == "Civilian labor force_Unemployed_Number" ~ "unemployed",
      Parameter == "Civilian labor force_Unemployed_Percent of labor force" ~ "unemployed rate",
      Parameter == "Not in labor force_Unemployed_Percent of labor force" ~ "not in labor force",
      TRUE ~ Parameter
    ),
    Ethnicity = "All",               # or any label you want
    Age = "16 years and over",       # from your table
    Sheet = "cpsaat02"               # or the actual sheet name
  ) %>%
  select(Year, Ethnicity, Gender, Age, Parameter, Value, Sheet)






# 1. Read the Excel sheet
cpsaat03 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat03")
# (Adjust skip or range if needed)

# 2. Process block headers to create Ethnicity, Gender, and Age columns
cpsaat03_clean <- cpsaat03 %>%
  # Rename the first column for easier reference
  rename(Label = `Age, sex, and race`) %>%
  
  # Identify Ethnicity rows (formerly "Race")
  mutate(
    Ethnicity_tmp = case_when(
      Label %in% c("TOTAL", "WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN") ~ Label,
      TRUE ~ NA_character_
    )
  ) %>%
  fill(Ethnicity_tmp, .direction = "down") %>%
  
  # Identify Gender rows
  mutate(
    Gender_tmp = case_when(
      Label %in% c("Men", "Women") ~ Label,
      TRUE ~ NA_character_
    )
  ) %>%
  fill(Gender_tmp, .direction = "down") %>%
  
  # Remove block-header-only rows (i.e. rows that are only headers for Ethnicity or Gender)
  filter(!Label %in% c("TOTAL", "WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "Men", "Women")) %>%
  
  # Rename remaining Label to Age
  rename(Age = Label) %>%
  
  # Convert Ethnicity_tmp "TOTAL" to "All Ethnicities" (or any label you prefer),
  # and if Gender_tmp is NA, assign "All"
  mutate(
    Ethnicity = case_when(
      Ethnicity_tmp == "TOTAL" ~ "All Ethnicities",
      TRUE ~ Ethnicity_tmp
    ),
    Gender = case_when(
      is.na(Gender_tmp) ~ "All",
      TRUE ~ Gender_tmp
    )
  ) %>%
  # Drop temporary columns
  select(Age, Ethnicity, Gender, everything(), -Ethnicity_tmp, -Gender_tmp)

# 3. Pivot the wide columns into long format
cpsaat03_long <- cpsaat03_clean %>%
  pivot_longer(
    cols = starts_with("2024_"),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(
    Year = 2024,
    Parameter = case_when(
      Parameter == "2024_Civilian noninsti- tutional population" ~ "population",
      Parameter == "2024_Civilian labor force_Total" ~ "labor force",
      Parameter == "2024_Civilian labor force_Percent of population" ~ "labor force participation rate",
      Parameter == "2024_Civilian labor force_Employed_Total" ~ "employed",
      Parameter == "2024_Civilian labor force_Employed_Percent of population" ~ "employed population ratio",
      Parameter == "2024_Civilian labor force_Unemployed_Number" ~ "unemployed",
      Parameter == "2024_Civilian labor force_Unemployed_Percent of labor force" ~ "unemployed rate",
      Parameter == "2024_Not in labor force_Unemployed_Percent of labor force" ~ "not in labor force",
      TRUE ~ Parameter
    ),
    Sheet = "cpsaat03"
  ) %>%
  select(Year, Ethnicity, Gender, Age, Parameter, Value, Sheet)





cpsaat04 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat04")

cpsaat04_clean <- cpsaat04 %>%
  # Rename the first column for convenience
  rename(Label = `Age and sex`) %>%
  
  # 1) Identify the ethnicity block
  mutate(
    Ethnicity_tmp = case_when(
      Label == "HISPANIC OR LATINO ETHNICITY" ~ Label,
      TRUE ~ NA_character_
    )
  ) %>%
  fill(Ethnicity_tmp, .direction = "down") %>%
  
  # 2) Identify gender rows
  mutate(
    Gender_tmp = case_when(
      Label %in% c("Men", "Women") ~ Label,
      TRUE ~ NA_character_
    )
  ) %>%
  fill(Gender_tmp, .direction = "down") %>%
  
  # 3) Filter out the pure block-header rows
  filter(!Label %in% c("HISPANIC OR LATINO ETHNICITY", "Men", "Women")) %>%
  
  # 4) Rename the remaining label text to "Age"
  rename(Age = Label) %>%
  
  # 5) Convert the Ethnicity label
  mutate(
    Ethnicity = case_when(
      Ethnicity_tmp == "HISPANIC OR LATINO ETHNICITY" ~ "Hispanic or Latino",
      TRUE ~ Ethnicity_tmp  # if you had other blocks, they'd appear here
    ),
    # If Gender_tmp is NA, that block was "All" (i.e., total)
    Gender = case_when(
      is.na(Gender_tmp) ~ "All",
      TRUE ~ Gender_tmp
    )
  ) %>%
  
  # 6) Drop the temporary columns
  select(Age, Ethnicity, Gender, everything(), -Ethnicity_tmp, -Gender_tmp)


cpsaat04_long <- cpsaat04_clean %>%
  pivot_longer(
    cols = starts_with("2024_"),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(
    # Add the year (since all columns are 2024)
    Year = 2024,
    # Rename parameter strings to short, descriptive labels
    Parameter = case_when(
      Parameter == "2024_Civilian noninsti- tutional population" ~ "population",
      Parameter == "2024_Civilian labor force_Total" ~ "labor force",
      Parameter == "2024_Civilian labor force_Percent of population" ~ "labor force participation rate",
      Parameter == "2024_Civilian labor force_Employed_Total" ~ "employed",
      Parameter == "2024_Civilian labor force_Employed_Percent of population" ~ "employed population ratio",
      Parameter == "2024_Civilian labor force_Unemployed_Number" ~ "unemployed",
      Parameter == "2024_Civilian labor force_Unemployed_Percent of labor force" ~ "unemployed rate",
      Parameter == "2024_Not in labor force_Unemployed_Percent of labor force" ~ "not in labor force",
      TRUE ~ Parameter
    ),
    Sheet = "cpsaat04"  # or your preferred label
  ) %>%
  select(Year, Ethnicity, Gender, Age, Parameter, Value, Sheet)






cpsaat05 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat05")

cpsaat05_clean <- cpsaat05 %>%
  rename(Label = `Employment status, sex, and age_TOTAL`) %>%
  # 1) Identify block headers: lines that contain "Men," or "Women," or "Both sexes,"
  mutate(
    is_block_header = str_detect(Label, "^Men,|^Women,|^Both sexes,")
  ) %>%
  # 2) Extract Gender + Age from those headers
  #    For example: "Men, 16 years and over" => Gender = "Men", Age = "16 years and over"
  mutate(
    GenderAge = if_else(is_block_header, Label, NA_character_)
  ) %>%
  fill(GenderAge, .direction = "down") %>%
  # 3) Remove the rows that are purely block headers
  filter(!is_block_header) %>%
  # 4) Now parse out Gender + Age from the "GenderAge" string
  #    E.g. "Men, 16 years and over" => Gender = "Men", Age = "16 years and over"
  #    We'll do a simple approach using a regular expression or str_split.
  separate(
    GenderAge,
    into = c("Gender", "Age"),
    sep = ", ",
    extra = "merge"   # "Men, 16 years and over" -> c("Men", "16 years and over")
  ) %>%
  # 5) The leftover "Label" lines become the Parameter
  rename(Parameter = Label) %>%
  select(Gender, Age, Parameter, everything(), -is_block_header)


cpsaat05_long <- cpsaat05_clean %>%
  pivot_longer(
    cols = -c(Gender, Age, Parameter),  # pivot everything except these
    names_to = "ColumnName",
    values_to = "Value"
  ) %>%
  # Now parse out Race + Year from ColumnName, e.g. "White_2023_TOTAL"
  # We'll use a small regex or str_split. For example:
  # "Total_2023_TOTAL" => Race = "Total", Year = "2023"
  # "Black or African American_2024_TOTAL" => Race = "Black or African American", Year = "2024"
  separate(
    ColumnName,
    into = c("Race", "Year", "Discard"),  # we have an extra "TOTAL" piece
    sep = "_",
    extra = "merge",  # if "Black or African American" has spaces, keep them together
    fill = "right"    # ensure we get Year in the second piece, if present
  ) %>%
  # Clean up Race/Year
  mutate(
    # If Race == "Total", interpret that as "All Races" (or keep "Total" if you prefer).
    Race = if_else(Race == "Total", "All Races", Race),
    Year = as.numeric(Year)   # convert "2023" to numeric 2023
  ) %>%
  select(Year, Race, Gender, Age, Parameter, Value) %>%
  # Optionally add a Sheet column
  mutate(Sheet = "cpsaat05") %>%
  select(Year, Race, Gender, Age, Parameter, Value, Sheet)










cpsaat06 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat06")

cpsaat06_clean <- cpsaat06 %>%
  rename(Label = `Employment status, sex, and age`) %>%
  
  # 1) Flag rows that are block headers
  mutate(
    is_block_header = str_detect(Label, "^Men,|^Women,|^Both sexes,")
  ) %>%
  
  # 2) Create a GenderAge column, filling down
  mutate(
    GenderAge = if_else(is_block_header, Label, NA_character_)
  ) %>%
  fill(GenderAge, .direction = "down") %>%
  
  # 3) Remove rows that are purely block headers
  filter(!is_block_header) %>%
  
  # 4) Separate "Men, 16 years and over" → (Gender = "Men") + (Age = "16 years and over")
  separate(
    GenderAge,
    into = c("Gender", "Age"),
    sep = ", ",
    extra = "merge"  # If there's more text, keep it in the second piece
  ) %>%
  
  # 5) Rename the leftover label to "Parameter"
  rename(Parameter = Label) %>%
  select(Gender, Age, Parameter, everything(), -is_block_header)

cpsaat06_long <- cpsaat06_clean %>%
  pivot_longer(
    cols = -c(Gender, Age, Parameter),  # pivot all columns except these
    names_to = "ColumnName",
    values_to = "Value"
  ) %>%
  # Separate e.g. "Hispanic or Latino ethnicity_Total(1)_2023" 
  # into 3 pieces: "Hispanic or Latino ethnicity", "Total(1)", "2023"
  separate(
    ColumnName,
    into = c("EthPrefix", "EthSubgroup", "Year"),
    sep = "_",
    extra = "merge",  # so "Puerto Rican" stays together
    fill = "right"
  ) %>%
  # Convert Year to numeric, fix up Ethnicity label
  mutate(
    Year = as.numeric(Year),
    # If you want to rename "Total(1)" to "Total", do so:
    EthSubgroup = if_else(EthSubgroup == "Total(1)", "Total", EthSubgroup),
    # Combine the prefix with the subgroup if you like, or just keep the subgroup
    Ethnicity = paste("Hispanic or Latino -", EthSubgroup)
  ) %>%
  # Drop the columns we no longer need
  select(Year, Ethnicity, Gender, Age, Parameter, Value) %>%
  # Finally, add a Sheet column
  mutate(Sheet = "cpsaat06") %>%
  select(Year, Ethnicity, Gender, Age, Parameter, Value, Sheet)








cpsaat07 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat07")

cpsaat07_clean <- cpsaat07 %>%
  rename(Label = `Employment status, sex, race, and Hispanic or Latino ethnicity`) %>%
  
  # 1) Mark rows that are block headers
  mutate(
    block = case_when(
      Label %in% c("TOTAL", "Men", "Women", "White", 
                   "Black or African American", "Asian", 
                   "Hispanic or Latino ethnicity") ~ Label,
      TRUE ~ NA_character_
    )
  ) %>%
  # 2) Fill block down so all subsequent rows get the same group
  fill(block, .direction = "down") %>%
  
  # 3) Remove the rows that are purely block headers
  filter(!Label %in% c("TOTAL", "Men", "Women", "White", 
                       "Black or African American", "Asian", 
                       "Hispanic or Latino ethnicity")) %>%
  
  # 4) Rename leftover label text to "Parameter"
  rename(Parameter = Label) %>%
  
  # 5) From the block, assign Race and Gender
  mutate(
    Race = case_when(
      block == "TOTAL" ~ "All",
      block == "Men" ~ "All",
      block == "Women" ~ "All",
      block == "White" ~ "White",
      block == "Black or African American" ~ "Black or African American",
      block == "Asian" ~ "Asian",
      block == "Hispanic or Latino ethnicity" ~ "Hispanic or Latino",
      TRUE ~ "All"
    ),
    Gender = case_when(
      block == "TOTAL" ~ "All",
      block == "Men" ~ "Men",
      block == "Women" ~ "Women",
      block == "White" ~ "All",
      block == "Black or African American" ~ "All",
      block == "Asian" ~ "All",
      block == "Hispanic or Latino ethnicity" ~ "All",
      TRUE ~ "All"
    )
  ) %>%
  select(Race, Gender, Parameter, everything(), -block)

cpsaat07_long <- cpsaat07_clean %>%
  pivot_longer(
    cols = -c(Race, Gender, Parameter),  # pivot all columns except these
    names_to = "ColumnName",
    values_to = "Value"
  ) %>%
  # Separate out "2024" from the rest
  separate(
    ColumnName,
    into = c("Year", "Education"),
    sep = "_",
    extra = "merge",  # keep the rest in "Education"
    fill = "right"
  ) %>%
  # Convert Year to numeric, if all are 2024
  mutate(
    Year = as.numeric(Year),
    # Optionally, clean up the Education text:
    # e.g. remove parentheses or rename "Bachelor's degree and higher" to "Bachelor or higher"
    Education = str_replace_all(Education, "\\(1\\)|\\(2\\)", "") %>%
      str_trim()
  ) %>%
  # Add a Sheet column for clarity
  mutate(Sheet = "cpsaat07") %>%
  select(Year, Race, Gender, Education, Parameter, Value, Sheet)







cpsaat08 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat08")

cpsaat08_clean <- cpsaat08 %>%
  # Rename the first column for convenience
  rename(Label = `Age, sex, race, and Hispanic or Latino ethnicity`) %>%
  
  # Mark rows that are one of the known block headers
  mutate(
    block = case_when(
      Label %in% c("TOTAL", "White", "Black or African American", 
                   "Asian", "Hispanic or Latino ethnicity") ~ Label,
      TRUE ~ NA_character_
    )
  ) %>%
  # Fill block down
  fill(block, .direction = "down") %>%
  
  # Remove the rows that are purely these block headers
  filter(!Label %in% c("TOTAL", "White", "Black or African American", 
                       "Asian", "Hispanic or Latino ethnicity")) %>%
  
  # Create a Race column from block
  mutate(
    Race = case_when(
      block == "TOTAL" ~ "All",
      block == "White" ~ "White",
      block == "Black or African American" ~ "Black or African American",
      block == "Asian" ~ "Asian",
      block == "Hispanic or Latino ethnicity" ~ "Hispanic or Latino",
      TRUE ~ "All"
    )
  ) %>%
  select(Race, Label, everything(), -block)

cpsaat08_clean <- cpsaat08_clean %>%
  mutate(
    Gender = case_when(
      str_starts(Label, "Men, ") ~ "Men",
      str_starts(Label, "Women, ") ~ "Women",
      str_starts(Label, "Total, ") ~ "All",
      TRUE ~ "All"
    ),
    Age = case_when(
      str_starts(Label, "Men, ") ~ str_remove(Label, "^Men,\\s*"),
      str_starts(Label, "Women, ") ~ str_remove(Label, "^Women,\\s*"),
      str_starts(Label, "Total, ") ~ str_remove(Label, "^Total,\\s*"),
      TRUE ~ Label
    )
  ) %>%
  # The leftover lines in Label are now parsed into Age, so we can drop Label
  select(Race, Gender, Age, everything(), -Label)

cpsaat08_long <- cpsaat08_clean %>%
  pivot_longer(
    cols = starts_with("2024_"),
    names_to = "ColumnName",
    values_to = "Value"
  ) %>%
  # Separate "2024_Employed(1)_Full-time workers_Total" => "2024" + "Employed(1)_Full-time workers_Total"
  separate(
    ColumnName,
    into = c("Year", "Parameter"),
    sep = "_",
    extra = "merge",
    fill = "right"
  ) %>%
  mutate(
    Year = as.numeric(Year),    # "2024" -> 2024
    Sheet = "cpsaat08"
  ) %>%
  select(Year, Race, Gender, Age, Parameter, Value, Sheet)





cpsaat09 <- read_excel("cpsa2024_cleaned.xlsx", sheet = "cpsaat09")

cpsaat09_long <- cpsaat09 %>%
  pivot_longer(
    cols = -Occupation,      # pivot all columns except the Occupation column
    names_to = "ColumnName",
    values_to = "Value"
  ) %>%
  # Split the column name, e.g. "Total_16 years and over_2023"
  # into (Gender, Age, Year)
  separate(
    ColumnName,
    into = c("Gender", "Age", "Year"),
    sep = "_",
    extra = "merge",  # if there's leftover text, keep it in the last piece
    fill = "right"    # if not enough pieces, fill from the right
  ) %>%
  # Convert "Year" to numeric (2023, 2024)
  mutate(
    Year = as.numeric(Year),
    # Rename "Total" to "All" for the Gender column
    Gender = if_else(Gender == "Total", "All", Gender)
  ) %>%
  # Rename Occupation -> Parameter
  rename(Parameter = Occupation) %>%
  # Add a Sheet column
  mutate(Sheet = "cpsaat09") %>%
  select(Year, Gender, Age, Parameter, Value, Sheet)






data_list <- lapply(data_list, function(df) {
  # Ensure Education exists (if missing, add it)
  if (!"Education" %in% names(df)) {
    df$Education <- NA
  }
  
  # Convert Year to numeric for consistency
  df$Year <- as.numeric(as.character(df$Year))
  
  # If a data frame has a "Race" column but no "Ethnicity", rename "Race" to "Ethnicity"
  if ("Race" %in% names(df) && !"Ethnicity" %in% names(df)) {
    df <- dplyr::rename(df, Ethnicity = Race)
  }
  # If both "Race" and "Ethnicity" exist, drop the "Race" column
  if ("Race" %in% names(df) && "Ethnicity" %in% names(df)) {
    df <- dplyr::select(df, -Race)
  }
  
  # Ensure Ethnicity exists; if not, create it with "All"
  if (!"Ethnicity" %in% names(df)) {
    df$Ethnicity <- "All"
  } else {
    # Replace NA values in Ethnicity with "All"
    df$Ethnicity <- if_else(is.na(df$Ethnicity), "All", df$Ethnicity)
  }
  
  # Replace NA values in Education with "All"
  df$Education <- if_else(is.na(df$Education), "All", df$Education)
  
  df
})



# Merge all standardized data frames into one
merged_data <- dplyr::bind_rows(data_list)

# Optionally, inspect the merged data:
head(merged_data)


