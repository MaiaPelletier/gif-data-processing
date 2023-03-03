# Indicator 8.9.1 ------------------------------------------------------
# Tourism direct GDP as a proportion of total GDP and in growth rate

library(dplyr)
library(cansim)


national_gdp <- get_cansim("36-10-0434-03", factors = FALSE)
tourism_gdp <- get_cansim("36-10-0234-01", factors = FALSE)
# tourism_share <- get_cansim("36-10-0235-01", factors = FALSE)

national_gdp <- 
  national_gdp %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2014,
    `Seasonal adjustment` == "Seasonally adjusted at annual rates",
    Prices == "Chained (2012) dollars",
    `North American Industry Classification System (NAICS)` == "All industries [T001]"
  ) %>% 
  select(
    REF_DATE,
    VALUE
  ) %>% 
  mutate(
    Year = substr(REF_DATE, 1, 4)
  ) %>% 
  group_by(Year) %>% 
  summarise(GDP = mean(VALUE), .groups = "drop")


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
    Year = substr(REF_DATE, 1, 4)  
  ) %>% 
  group_by(Year) %>% 
  summarise(Tourism_GDP = sum(VALUE), .groups = "drop") 
  # mutate(Value = round(((Value / lag(Value)) - 1) * 100, 2)) %>% 
  # filter(Year > 2014)

data_final <- 
  left_join(tourism_gdp, national_gdp) %>% 
  transmute(
    Year, 
    Value = round((Tourism_GDP/GDP)*100, 2)
  )

write.csv(
  data_final,
  "data/indicator_8-9-1.csv",
  na = "",
  row.names = FALSE
)

