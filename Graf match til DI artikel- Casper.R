library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(ggplot2)

##Forbrugertillidsindikator (Erstat fil placering med egen)
df <- read_excel("C:/Users/helen/Downloads/DST data - DI forbrugertillidsindaktor.xlsx", skip = 2)
names(df)[1] <- "category"

indikator_quarterly <- df %>%
  pivot_longer(-category, names_to = "period", values_to = "value") %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    month   = as.integer(str_sub(period, 6, 7)),
    quarter = as.yearqtr(as.Date(sprintf("%d-%02d-01", year, month)))
  ) %>%
  group_by(category, quarter) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(quarter) %>%
  summarise(indikator = mean(value, na.rm = TRUE), .groups = "drop")

##Privatforbrug: quarterly QoQ growth -> rolling annual growth
pf <- read_excel("C:/Users/helen/Downloads/DST data - Privatforbrug realvækst 2000 - 2016.xlsx", skip = 2)

pf_long <- pf %>%
  select(-(1:3)) %>%                       # drop the 3 label columns
  pivot_longer(everything(), names_to = "period", values_to = "qoq") %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    q       = as.integer(str_sub(period, 6, 6)),
    quarter = as.yearqtr(year + (q - 1) / 4)
  ) %>%
  arrange(quarter) %>%
  mutate(
    # compound the last 4 quarters of QoQ growth into an annual (YoY) rate
    annual_growth = (rollapply(1 + qoq / 100, 4, prod, align = "right", fill = NA) - 1) * 100
  )

##Merge the two series
plot_data <- indikator_quarterly %>%
  inner_join(pf_long %>% select(quarter, annual_growth), by = "quarter")

##Plot, dual axis like the DI chart
scale_factor <- 25 / 8   # left axis (-25..25) vs right axis (-8..8)

ggplot(plot_data, aes(x = quarter)) +
  geom_col(aes(y = annual_growth * scale_factor),
           fill = "#4FA8D8", width = 0.2) +
  geom_line(aes(y = indikator), color = "grey30", linewidth = 1) +
  scale_y_continuous(
    name = "Nettotal",
    limits = c(-25, 25),
    breaks = seq(-25, 25, 8.33),
    sec.axis = sec_axis(~ . / scale_factor, name = "Pct.",
                        breaks = seq(-8, 8, 3))
  ) +
  scale_x_yearqtr(format = "%y", n = 17) +
  labs(
    title = "DI's Forbrugertillidsindikator & Privatforbrug",
    subtitle = "Forsøg på at matche artiklen",
    caption = "Udarbejdet i R",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
