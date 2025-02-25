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