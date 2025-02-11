library(dplyr)
library(DBI)
library(RSQLite)
library(tidyr)
library(readxl)
library(purrr)
library (fredr)
library(ggplot2)
library(lubridate)

fredr_set_key(trimws(Sys.getenv("SP500_APIKEY")))

sp500_data <- fredr(series_id="SP500")
head(sp500_data)

sp500_df <- data.frame(Date = sp500_data$date, Price = sp500_data$value)
head(sp500_df)

ggplot(sp500_df, aes(x = Date, y = Price)) +
  geom_line(color = "blue") +
  labs(title = "S&P 500 Historical Data",
       x = "Date",
       y = "S&P 500 Index Value") +
  theme_minimal()

sp500_recent <- fredr(
  series_id = "SP500",
  observation_start = as.Date(Sys.Date()) - 5 * 365
)

head(sp500_recent)



ggplot(sp500_df, aes(x = Date, y = Price)) +
  geom_line(color = "blue", size = 1) +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +  # Improve date formatting
  labs(title = "S&P 500 Historical Data",
       subtitle = "All available data from FRED",
       x = "Year",
       y = "S&P 500 Index Value") +
  theme_minimal()


# Retrieve last 5 years of S&P 500 data
sp500_recent <- fredr(
  series_id = "SP500",
  observation_start = as.Date(Sys.Date()) - 5 * 365  # Last 5 years
)

# Convert to DataFrame
sp500_recent_df <- data.frame(Date = sp500_recent$date, Price = sp500_recent$value)

# Plot recent S&P 500 data
ggplot(sp500_recent_df, aes(x = Date, y = Price)) +
  geom_line(color = "red", size = 1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +  # Show every 6 months
  labs(title = "S&P 500 Recent Performance",
       subtitle = "Last 5 Years",
       x = "Date",
       y = "S&P 500 Index Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

