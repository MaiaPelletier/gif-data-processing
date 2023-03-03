# Indicator 17.1.2 ---------------------------------------------------------
# Proportion of domestic budget funded by domestic taxes

library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

finance_stats <- get_cansim("10-10-0147-01", factors = FALSE)

geocodes <- read.csv("geocodes.csv")

cgfs <- c(
  "Taxes [11]",
  "Expense [2]",
  "Consumption of fixed capital [23]",
  "Memorandum items, capitalized research and development costs",
  "Memorandum items, consumption of fixed capital according to public sector accounts",
  "Memorandum items, nonfinancial assets according to public sector accounts"
  )

data_final <- 
  finance_stats %>%
  filter(
    `Display value` == "Stocks",
    GEO == "Canada",
    `Public sector components` == "Consolidated Canadian general government",
    `Statement of operations and balance sheet` %in% cgfs
  ) %>%
  select(
    Year = REF_DATE,
    CGFS = `Statement of operations and balance sheet`,
    Value = VALUE
  ) %>% 
  mutate(
    CGFS = str_remove(CGFS, " \\[[0-9]*\\]")
    ) %>% 
  pivot_wider(
    names_from = "CGFS",
    values_from = "Value"
  ) %>% 
  rename(
    NFA = `Memorandum items, nonfinancial assets according to public sector accounts`
  ) %>% 
  transmute(
    Year, A = Taxes,
    B = Expense - `Consumption of fixed capital` + `Memorandum items, consumption of fixed capital according to public sector accounts` + `Memorandum items, capitalized research and development costs`,
    C = NFA - lag(NFA)
  ) %>% 
  transmute(
    Year,
    Value = round((A/(B + C)) * 100, 2)
  ) %>% 
  filter(Year >= 2015)
 
write.csv(
  data_final, 
  "data/indicator_17-1-2.csv",
  row.names = FALSE,
  na = ""
)
