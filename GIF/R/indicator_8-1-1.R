# Indicator 8.1.1 ------------------------------------------------------
# Annual growth rate of real GDP per capita

library(dplyr)
library(cansim)
library(stringr)

national_gdp <- get_cansim("36-10-0434-03", factors = FALSE)
pt_gdp <- get_cansim("36-10-0402-01", factors = FALSE)
pop_ests <- get_cansim("17-10-0005-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

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
    Geography = GEO,
    VALUE
  ) %>% 
  mutate(
    Year = substr(REF_DATE, 1, 4)
  ) %>% 
  group_by(Year, Geography) %>% 
  summarise(GDP = (mean(VALUE)) * 1000000, .groups = "drop")

pt_gdp <-
  pt_gdp %>% 
  filter(
    REF_DATE >= 2014,
    Value == "Chained (2012) dollars",
    `North American Industry Classification System (NAICS)` == "All industries [T001]"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    GDP = VALUE
  ) %>% 
  mutate(GDP = GDP * 1000000)

pop_ests <- 
  pop_ests %>% 
  filter(
    REF_DATE >= 2014,
    Sex == "Both sexes",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Population = VALUE
  )

all_gdp <- 
  bind_rows(
    national_gdp,
    pt_gdp
  ) %>% 
  left_join(pop_ests) %>% 
  mutate(
    gdp_per_cap = GDP/Population
  ) %>% 
  arrange(Geography, Year) %>% 
  group_by(Geography) %>% 
  mutate(
    Value = (gdp_per_cap - lag(gdp_per_cap)) / lag(gdp_per_cap),
    Value = round(Value * 100, 2),
    Geography = recode(Geography, Canada = "")
  ) %>% 
  filter(Year > 2014) %>% 
  select(-c(3:5)) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = "Value")

write.csv(
  all_gdp,
  "data/indicator_8-1-1.csv",
  na = "",
  row.names = FALSE
)

