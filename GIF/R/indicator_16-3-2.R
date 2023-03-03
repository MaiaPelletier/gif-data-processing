library(dplyr)
library(cansim)
library(tidyr)

young_persons <- get_cansim("35-10-0003-01", factors = FALSE)
adults <- get_cansim("35-10-0154-01", factors = FALSE)
offenders <- get_cansim("35-10-0155-01", factors = FALSE)

total_prison_population <- bind_rows(
    
    adults %>% 
      filter(
        GEO == "Provinces and Territories",
        `Custodial and community supervision` %in% c("Total actual-in count", "Other statuses, actual-in count")
      ) %>% 
      select(REF_DATE, category = `Custodial and community supervision`, VALUE) %>% 
      pivot_wider(names_from = "category", values_from = VALUE) %>% 
      transmute(
        REF_DATE, Population = "PT_Adults",
        Value = `Total actual-in count` - `Other statuses, actual-in count`
      ),
  
  offenders %>% 
    filter(
      GEO == "Canada",
      `Custodial and community supervision` == "Actual-in count"
    ) %>% 
    select(REF_DATE, Value = VALUE) %>% 
    mutate(Population = "Federal_Adults"),
  
  young_persons %>% 
    filter(
      GEO == "Provinces and Territories",
      `Custodial and community supervision` %in% c("Total actual-in count", "Sentenced open custody, actual-in count")
    ) %>% 
    select(REF_DATE, category = `Custodial and community supervision`, VALUE) %>% 
    pivot_wider(names_from = "category", values_from = VALUE) %>% 
    transmute(
      REF_DATE, Population = "Youth",
      Value = `Total actual-in count` - `Sentenced open custody, actual-in count`
    )
  ) %>% 
  group_by(REF_DATE) %>% 
  summarise(Total_prison_pop = sum(Value))


unsentenced_population <-
  bind_rows(
    adults %>% 
      filter(
        GEO == "Provinces and Territories",
        `Custodial and community supervision` == "Remand, actual-in count"
      ) %>% 
      select(REF_DATE, category = `Custodial and community supervision`, VALUE),
  
    young_persons %>% 
      filter(
        GEO == "Provinces and Territories",
        `Custodial and community supervision` == "Pre-trial detention and other temporary detention, actual-in count"
      ) %>% 
      select(REF_DATE, category = `Custodial and community supervision`, VALUE)
  ) %>% 
  group_by(REF_DATE) %>% 
  summarise(Unsentenced_pop = sum(VALUE))

left_join(unsentenced_population, total_prison_population) %>% 
  filter(substr(REF_DATE, 1, 4) >= 2014) %>% 
  rename(Year = REF_DATE) %>% 
  transmute(Year, Value = round((Unsentenced_pop/Total_prison_pop) * 100, 2)) %>% 
  write.csv("data/indicator_16-3-2.csv", na = "", row.names = FALSE)


