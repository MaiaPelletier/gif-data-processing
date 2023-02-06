# Indicator 8.6.1 ------------------------------------------------------
# Proportion of youth (aged 15-24 years) not in education, employment or
# training

library(dplyr)
library(cansim)
library(stringr)

NEET <- get_cansim("37-10-0196-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

selected_status <- c(
  "Sub-total, not in employment, education or training (NEET)",
  "Unemployed, Not in employment, education or training (NEET)",
  "Not in the labour force, Not in employment, education or training (NEET)"
)

NEET <- 
  NEET %>% 
  filter(
    REF_DATE >= 2015,
    GEO != "Organisation for Economic Co-operation and Development (OECD) - average",
    `Labour force and education status` %in% selected_status,
    Statistics == "Proportion"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Educational attainment level`,
    Status = `Labour force and education status`,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>% 
  mutate(
    Status = str_replace(Status, "Sub-total", "Total")
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

data_final <- bind_rows(
  # total line
  NEET %>%
    filter(Geography == "Canada",
           `Educational attainment level` == "Total, all education levels",
           Status == "Total, not in employment, education or training (NEET)",
           Sex == "Both sexes",
           `Age group` == "Total, 15 to 29 years") %>%
    mutate_at(2:6, ~ ""),
  
  NEET %>%
    filter(
      !(
        Geography == "Canada" &
          `Educational attainment level` == "Total, all education levels" &
          Status == "Total, not in employment, education or training (NEET)" &
          Sex == "Both sexes" &
          `Age group` == "Total, 15 to 29 years"
      )
    )
)

write.csv(
  data_final,
  "data/indicator_8-6-1.csv",
  na = "",
  row.names = FALSE
)

