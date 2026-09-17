#Opgave 2
library(readxl)
df <- read_excel(
  "C:/Users/helen/Downloads/DST data - DI forbrugertillidsindaktor.xlsx",
  skip = 2
)

names(df)[1] <- "category"

View(df)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)      # for as.yearqtr, handy for plotting/sorting

# 2. Wide -> long: one row per category x month
df_long <- df %>%
  pivot_longer(cols = -category, names_to = "period", values_to = "value")

# 3. Parse "2000M01" -> year, month, quarter
df_long <- df_long %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    month   = as.integer(str_sub(period, 6, 7)),
    date    = as.Date(sprintf("%d-%02d-01", year, month)),
    quarter = as.yearqtr(date)
  )

# 4. Monthly -> quarterly, per category (mean of the 3 months)
df_quarterly <- df_long %>%
  group_by(category, quarter) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# 5. Average across the 4 categories -> single quarterly indicator
indikator_quarterly <- df_quarterly %>%
  group_by(quarter) %>%
  summarise(forbrugertillidsindikator = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(quarter)

print(indikator_quarterly)
