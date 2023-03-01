library(tidyverse)
library(readxl)
path <- "C:/Users/pellmai/Downloads/"
geocodes <- read_csv("geocodes.csv")
abbrevs <- read_csv("geo_abbreviations.csv")


sectors <- c(
  "GHG TOTAL",
  "NATIONAL GHG TOTAL",
  "Oil and Gas",
  "Electricity",
  "Transport",
  "Heavy Industry",
  "Buildings",
  "Agriculture",
  "Waste",
  "Coal Production",
  "Light Manufacturing, Construction and Forest Resources"
)

pt_file <- file.path(path, "EN_GHG_Econ_Prov_Terr.xlsx")
can_file <- file.path(path, "EN_GHG_Econ_Canada.xlsx")


# prov data -----------

prov_econ_data <- function(num) {

  pt_name <- excel_sheets(pt_file)[num]
  
  read_excel(pt_file, sheet = num, skip = 2) %>% 
    slice(-c(1, (nrow(.)-8):nrow(.))) %>% 
    rename_at(1, ~ "Sector") %>% 
    filter(Sector %in% sectors) %>% 
    pivot_longer(
      cols = 2:(ncol(.)),
      names_to = "Year",
      names_transform = list(Year = as.numeric),
      values_to = "Value",
      values_transform = list(Value = as.numeric)
    ) %>% 
    mutate(
      Geography = pt_name
    )
  
}

pts <- 3:16
pt_econ <- map_dfr(pts, prov_econ_data)

# national data -----------

nat <- 
  read_excel(can_file, sheet = 3, skip = 2) %>% 
  slice(-c(1, (nrow(.)-8):nrow(.))) %>%
  rename_at(1, ~ "Sector") %>% 
  filter(Sector %in% sectors) %>% 
  pivot_longer(
    cols = 2:(ncol(.)),
    names_to = "Year",
    names_transform = list(Year = as.numeric),
    values_to = "Value",
    values_transform = list(Value = as.numeric)
  ) %>% 
  mutate(
    Geography = "Canada"
  )


# combine -----------

bind_rows(nat, pt_econ) %>% 
  rename(Code = Geography) %>% 
  left_join(abbrevs, by = "Code") %>% 
  left_join(geocodes) %>% 
  select(Year, Geography, Sector, GeoCode, Value) %>% 
  mutate(
    Geography = ifelse(is.na(Geography), "Canada", Geography),
    Sector = case_when(
      Sector == "NATIONAL GHG TOTAL" ~ "",
      Sector == "GHG TOTAL" ~ "Provincial Inventory Total",
      TRUE ~ Sector
    ),
    Geography = ifelse(
      Sector == "" & Geography == "Canada",
      "",
      paste0("data.", Geography)
    ),
    Sector = ifelse(
      Sector != "",
      paste0("data.", Sector),
      Sector
    )
  ) %>% 
  rename_at(2:3, ~ paste0("data.", .x)) %>% 
  write_csv("data/13-1-1.csv", na = "")
  
  
  
  
  
  
  
  
  
  


















