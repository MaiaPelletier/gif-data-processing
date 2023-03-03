library(cansim)
library(dplyr)


cgfs_gov <- get_cansim("10-10-0147-01", factors = FALSE)
cgfs_pension <- get_cansim("10-10-0022-01", factors = FALSE)
gdp <- get_cansim("36-10-0222-01", factors = FALSE)

statements_gov <- c(
  "Taxes [11]",
  "Social contributions [12]",
  "Grants, revenue [13]",
  "Other revenue [14]"
)

cgfs_gov %>% 
  filter(
    REF_DATE >= 2015,
    GEO == "Canada",
    `Public sector components` == "Consolidated Canadian general government",
    `Display value` == "Stocks",
    `Statement of operations and balance sheet` %in% statements_gov
  ) %>% 
  select(
    Year = REF_DATE,
    VALUE
  ) %>% 
  group_by(Year) %>% 
  summarise(Total_Revenue = sum(VALUE))


cgfs_pension %>% 
  filter(
    REF_DATE >= 2015,
    `Display value` == "Stocks",
    `Statement of operations and balance sheet` %in% statements_gov
  ) %>% 
  select(
    Year = REF_DATE,
    VALUE
  ) %>% 
  group_by(Year) %>% 
  summarise(Total_Revenue_Pension = sum(VALUE))

gdp %>% 
  filter(
    REF_DATE >=2015,
    GEO == "Canada",
    Prices == "Current prices",
    Estimates == "Gross domestic product at market prices"
  ) %>% 
  select(REF_DATE, VALUE)
