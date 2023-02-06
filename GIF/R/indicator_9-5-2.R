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


researchers <- 
  left_join(
    
    # Researcher numbers
    rd_personnel %>%
      filter(
        REF_DATE >= 2010,
        `Occupational category` %in% c("Researchers", "On-site research consultants"),
        `Performing sector` == "Total performing sector"
      ) %>%
      select(
        Year = REF_DATE,
        #`Performing sector`,
        `Type of science`,
        `Occupational category`,
        No_Personnel = VALUE
      ) %>%
      group_by_at(1:3) %>%
      summarise(No_Personnel = sum(No_Personnel), .groups = "drop"),
    
    # Population estimate
    pop_ests %>% 
      filter(
        REF_DATE >= 2010,
        GEO == "Canada",
        Sex == "Both sexes",
        `Hierarchy for Age group` %in% age_hierarchy
      ) %>%
      select(
        Year = REF_DATE,
        Population = VALUE
      )
    ) %>%
    transmute(
      Year, `Type of science`,
      Value = (No_Personnel/Population)*1000000
    )



data_final <-
  bind_rows(
    researchers %>%
      filter(`Type of science` == "Total sciences") %>%
      mutate(across(2:(ncol(.)-1), ~ "")),
    researchers %>%
      filter(!(`Type of science` == "Total sciences"))
  )

write.csv(data_final, "data/indicator_9-5-2.csv", na = "", row.names = FALSE)

