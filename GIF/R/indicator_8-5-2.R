# Indicator 8.5.2 ------------------------------------------------------
# Unemployment rate, by sex, age and persons with disabilities

library(dplyr)
library(cansim)
# library(stringr)

lf_chars <- get_cansim("14-10-0327-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

lf_chars <- 
  lf_chars %>% 
  filter(
    REF_DATE >= 2015,
    `Labour force characteristics` == "Unemployment rate"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

data_final <- bind_rows(
  #total line
  lf_chars %>%
    filter(Geography == "Canada",
           Sex == "Both sexes",
           `Age group` == "15 years and over") %>%
    mutate_at(2:4, ~ ""),
  
  lf_chars %>%
    filter(!(
      Geography == "Canada" &
        Sex == "Both sexes" &
        `Age group` == "15 years and over"
    ))
)

write.csv(
  data_final,
  "data/indicator_8-5-2.csv",
  na = "",
  row.names = FALSE
)

