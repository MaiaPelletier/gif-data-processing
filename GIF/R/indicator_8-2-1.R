# Indicator 8.2.1 ------------------------------------------------------
# Annual growth rate of real GDP per employed person

library(dplyr)
library(cansim)
# library(stringr)

national_gdp <- get_cansim("36-10-0434-03", factors = FALSE)
labour_force <- get_cansim("14-10-0327-01", factors = FALSE)

national_gdp <-
  national_gdp %>% 
  filter(
    substr(REF_DATE, 1, 4) >= 2014,
    `Seasonal adjustment` == "Seasonally adjusted at annual rates",
    Prices == "2012 constant prices",
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
  summarise(GDP = (mean(VALUE)) * 1000000, .groups = "drop")

labour_force <- 
  labour_force %>% 
  filter(
    REF_DATE >= 2014,
    GEO == "Canada",
    `Labour force characteristics` == "Employment",
    Sex == "Both sexes",
    `Age group` == "15 years and over"
  ) %>% 
  select(
    Year = REF_DATE,
    Employment = VALUE
  )

data_final <- 
  left_join(national_gdp, labour_force) %>% 
  mutate(
    LabProd = GDP/Employment
  ) %>% 
  transmute(
    Year,
    Value = (LabProd - lag(LabProd)) / lag(LabProd),
    Value = round(Value * 100, 2)
  ) %>% 
  filter(Year > 2014)

write.csv(
  data_final,
  "data/indicator_8-2-1.csv",
  na = "",
  row.names = FALSE
)  
