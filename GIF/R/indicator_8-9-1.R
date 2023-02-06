# Indicator 8.9.1 ------------------------------------------------------
# Tourism direct GDP as a proportion of total GDP and in growth rate

library(dplyr)
library(cansim)

tourism_gdp <- get_cansim("36-10-0234-01", factors = FALSE)
tourism_share <- get_cansim("36-10-0235-01", factors = FALSE)

tourism_share <- 
  tourism_share %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2015
  ) %>% 
  select(
    REF_DATE,
    VALUE
  ) %>% 
  mutate(
    Year = substr(REF_DATE, 1, 4),
    Units = "Tourism direct GDP as a proportion of total GDP"
  ) %>% 
  group_by(Year, Units) %>% 
  summarise(Value = mean(VALUE), .groups = "drop")


tourism_gdp <- 
  tourism_gdp %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2014,
    `Seasonal adjustment` == "Seasonally adjusted at quarterly rates",
    Prices == "2012 constant prices",
    Activities == "Tourism gross domestic product (GDP)"
  ) %>% 
  select(
    REF_DATE,
    VALUE
  ) %>% 
  mutate(
    Year = substr(REF_DATE, 1, 4),
    Units = "Tourism direct GDP in growth rate"
  ) %>% 
  group_by(Year, Units) %>% 
  summarise(Value = mean(VALUE) * 1000000, .groups = "drop") %>% 
  mutate(Value = round(((Value / lag(Value)) - 1) * 100, 2)) %>% 
  filter(Year > 2014)
 
data_final <- bind_rows(
  tourism_share,
  tourism_gdp
)

write.csv(
  data_final,
  "data/indicator_8-9-1.csv",
  na = "",
  row.names = FALSE
)

