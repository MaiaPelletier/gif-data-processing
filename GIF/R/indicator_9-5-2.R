# Indicator 9.5.2 ------------------------------------------------------
# 9.5.2 Researchers (in full-time equivalent) per million inhabitants

library(cansim)
library(here)
library(dplyr)

rd_personnel <- get_cansim("27-10-0022-01", factors = FALSE)
pop_ests <- get_cansim("17-10-0005-01", factors = FALSE)


age_hierarchy <-
  c(
    "1.25",
    "1.31",
    "1.37",
    "1.43",
    "1.49",
    "1.55",
    "1.61",
    "1.67",
    "1.73",
    "1.79",
    "1.85",
    "1.86",
    "1.87",
    "1.88",
    "1.89",
    "1.126",
    "1.132",
    "1.138"
  )

rd_personnel %>% 
  distinct(GEO)

researchers <- 
  rd_personnel %>%
  filter(
    REF_DATE >= 2010,
    `Occupational category` %in% c("Researchers", "On-site research consultants"),
    `Performing sector` == "Total, performing sectors",
    `Type of science` == "Total sciences"
  ) %>%
  select(
    Year = REF_DATE,
    #`Performing sector`,
    `Occupational category`,
    No_Personnel = VALUE
  ) %>%
  group_by(Year) %>%
  summarise(No_Personnel = sum(No_Personnel), .groups = "drop")

population <- 
  pop_ests %>%
  filter(
    REF_DATE >= 2010,
    GEO == "Canada",
    Sex == "Both sexes",
    `Age group` %in% c("15 to 64 years", "65 years and over")
  ) %>%
  select(
    Year = REF_DATE,
    `Age group`,
    VALUE
  ) %>% 
  group_by(Year) %>% 
  summarise(Population = sum(VALUE), .groups = "drop")

data_final <- 
  left_join(researchers, population) %>% 
  transmute(Year, Value = (No_Personnel/Population)*1000000)

write.csv(data_final, "data/indicator_9-5-2.csv", na = "", row.names = FALSE)

