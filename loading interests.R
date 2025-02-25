library(rsdmx)
library(dplyr)
library(lubridate)
library(ggplot2)


interest_rates_path <- normalizePath(file.path("data", "macroeconomic", "interest-rates", "H15_data.xml"))

sdmxData <- readSDMX(interest_rates_path, isURL = FALSE)

df <- as.data.frame(sdmxData)

head(df)

# Map FREQ code to descriptive frequency
freq_map <- c("8" = "Daily", "9" = "Business Day")

df <- df %>% mutate(FREQ_desc = freq_map[as.character(FREQ)])

# For MATURITY, if "O" means Overnight:
df <- df %>% mutate(MATURITY_desc = ifelse(MATURITY == "O", "Overnight", MATURITY))

# You might also create mappings for INSTRUMENT and UNIT if needed.
df <- df %>% mutate(
  TIME_PERIOD = as.Date(TIME_PERIOD),
  OBS_VALUE = as.numeric(OBS_VALUE)
)



ggplot(df, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line() +
  labs(title = "Federal Funds Rate (Overnight)",
       x = "Date", y = "Interest Rate (%)")
