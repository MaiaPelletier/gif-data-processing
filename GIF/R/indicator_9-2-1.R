# 9.2.1 ------------------------------------------------------------------
library(cansim)
library(dplyr)

mva <- get_cansim("16-10-0117-01", factors = FALSE)
gdp <- get_cansim("36-10-0222-01", factors = FALSE)
pop <- get_cansim("17-10-0005-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

mva <- 
  mva %>%
  filter(
    REF_DATE > 2014,
    `Principal statistics` == "Manufacturing value added",
    `North American Industry Classification System (NAICS)` == "Manufacturing [31-33]"
  ) %>% 
  select(
    1:2, mva = VALUE
  ) 

gdp <- 
  gdp %>% 
  filter(
    REF_DATE > 2014,
    Prices == "Current prices",
    Estimates == "Gross domestic product at market prices"
  ) %>% 
  select(1:2, gdp = VALUE)

pop <- 
  pop %>% 
  filter(
    REF_DATE > 2014,
    Sex == "Both sexes",
    `Age group` == "All ages"
  ) %>% 
  select(1:2, pop = VALUE)

data_final <-
  bind_rows(
    mva %>% 
      left_join(gdp) %>% 
      filter(!is.na(gdp)) %>% 
      transmute(
        Year = REF_DATE, 
        Units = "Proportion of GDP",
        Geography = GEO,
        Value = mva/gdp
      ),
    mva %>% 
      left_join(pop) %>%
      filter(!is.na(pop)) %>% 
      transmute(
        Year = REF_DATE, 
        Units = "Per capita",
        Geography = GEO,
        Value = mva/pop
      )
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = "Value") %>% 
  mutate(Geography = ifelse(Geography == "Canada", "", Geography))


write.csv(
  data_final,
  "data/indicator_9-2-1.csv",
  row.names = FALSE,
  na = ""
)
