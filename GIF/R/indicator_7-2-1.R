# GIF 7.2.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)

# load CODR table from stc api
Raw_data <- get_cansim("25-10-0015-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

total_electricty <-
  Raw_data %>%
  filter(
    substr(REF_DATE, 1, 4) >= 2015,
    `Class of electricity producer` == "Total all classes of electricity producer",
    `Type of electricity generation` == "Total all types of electricity generation"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of electricity generation`,
    `Total generation` = VALUE
  ) %>%
  mutate(Year = substr(Year, 1, 4)) %>% 
  group_by(Year, Geography) %>% 
  summarise(Total = sum(`Total generation`), .groups = "drop")


renewable <- 
  Raw_data %>%
  filter(
    substr(REF_DATE, 1, 4) >= 2015,
    `Class of electricity producer` == "Total all classes of electricity producer",
    `Type of electricity generation` %in% c(
      # "Total hydro, tidal, wind, solar and other generation",
      "Hydraulic turbine",
      "Tidal power turbine",
      "Wind power turbine",
      "Solar",
      "Other types of electricity generation"
    )
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of electricity generation`,
    `Total renewable` = VALUE
  ) %>%
  mutate(Year = substr(Year, 1, 4)) %>% 
  group_by(Year, Geography, `Type of electricity generation`) %>% 
  summarise(Renewable = sum(`Total renewable`, na.rm = TRUE), .groups = "drop")


renewable_totals <- 
  renewable %>% 
    group_by(Year, Geography) %>% 
    summarise(Renewable = sum(Renewable), .groups = "drop") %>% 
    mutate(`Type of electricity generation` = "Total hydro, tidal, wind, solar and other generation")


renewable <- 
  bind_rows(renewable, renewable_totals) %>% 
  left_join(total_electricty, by = c("Year", "Geography")) %>% 
  transmute(
    Year, Geography, `Type of electricity generation`,
    Value = round((Renewable/Total)*100, 2)
  ) %>% 
  left_join(geocodes, by = "Geography") %>% 
  relocate(GeoCode, .before = Value)


total_line <-
  renewable %>%
  filter(
    Geography == "Canada",
    `Type of electricity generation` == "Total hydro, tidal, wind, solar and other generation",
  ) %>%
  mutate_at(2:(ncol(.) - 2), ~ "")

non_total_line <-
  renewable %>%
  filter(
    !(
      Geography == "Canada" &
        `Type of electricity generation` == "Total hydro, tidal, wind, solar and other generation"
    )
  )


final_data <- bind_rows(total_line, non_total_line) 


write.csv(final_data,
          "data/indicator_7-2-1.csv",
          na = "",
          row.names = FALSE)



