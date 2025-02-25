library(tidyverse)
# Define file paths
mortgages1_path <- file.path("data", "macroeconomic", "mortgages-rates", "nmdb-outstanding-mortgage-statistics-national-census-areas-quarterly.csv")
mortgages2_path <- file.path("data", "macroeconomic", "mortgages-rates", "nmdb-mortgage-performance-statistics-national-census-areas-quarterly.csv")

# Load the datasets
df1 <- read.csv(file1_path, stringsAsFactors = FALSE)
df2 <- read.csv(file2_path, stringsAsFactors = FALSE)

glimpse(df1)
glimpse(df2)

summary(df1)
summary(df2)

df1_cleaned <- df1 %>% drop_na(SERIESID, VALUE1)
df2_cleaned <- df2 %>% drop_na(SERIESID, VALUE1)

names(df1_cleaned) <- tolower(names(df1_cleaned))
names(df2_cleaned) <- tolower(names(df2_cleaned))

merged_df <- inner_join(df1_cleaned, df2_cleaned, 
                        by = c("year", "quarter", "geoid", "geoname", "market"),
                        suffix = c("_outstanding", "_performance"))

head(merged_df)
View(merged_df)  # Opens the dataset in RStudio's Data Viewer
