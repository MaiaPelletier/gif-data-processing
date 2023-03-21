library(tidyverse)
library(rvest)

rivers <- read_html("https://www.canada.ca/en/environment-climate-change/services/environmental-indicators/water-quality-canadian-rivers.html") 

# land use ----------------------------------------------------------------


rivers_land_use <- 
  rivers %>% 
  html_elements("table") %>% 
  .[2] %>% 
  html_table() %>% 
  .[[1]] %>% 
  select(1:4) %>% 
  rename_at(2:4, ~ trimws(str_remove(.x, "\\(.*\\)"))) %>% 
  mutate(
    Year = "2018-2020",
    Geography = "Canada"
  ) %>%
  pivot_longer(
    cols = Excellent:Fair,
    names_to = "Water quality category",
    values_to = "Value"
  )

rivers_land_use_total <- 
  rivers_land_use %>% 
  group_by(Year, Geography, `Land use category`) %>% 
  summarise(Value = sum(Value), `Water quality category` = "Total")


# regional ----------------------------------------------------------------

regional_rivers <-
  rivers %>% 
  html_elements("table") %>% 
  .[5] %>% 
  html_table() %>%
  .[[1]] %>%
  rename_at(2:6, ~ trimws(str_remove(.x, "\\(.*\\)"))) %>%
  mutate(
    Year = "2018-2020",
    `Land use category` = "Total"
  ) %>%
  pivot_longer(
    cols = 2:6,
    names_to = "Geography",
    values_to = "Value"
  ) %>% 
  filter(`Water quality category` %in% c("Excellent", "Good", "Fair"))

regional_rivers_total <- 
  regional_rivers %>% 
  group_by(Year, Geography, `Land use category`) %>% 
  summarise(Value = sum(Value), `Water quality category` = "Total")


# regional land use -------------------------------------------------------

regions <- pull(regional_rivers_total, Geography)
tbls <- seq(7, 15, by = 2)

get_regional_rivers <- function(i) {
  
  print(i)
  
  rivers %>% 
    html_elements("table") %>% 
    .[tbls[i]] %>% 
    html_table() %>%
    .[[1]] %>%
    select(1:4) %>%
    rename_at(2:4, ~ trimws(str_remove(.x, "\\(.*\\)"))) %>%
    mutate(
      Year = "2018-2020",
      Geography = regions[i]
    ) %>%
    pivot_longer(
      cols = Excellent:Fair,
      names_to = "Water quality category",
      values_to = "Value"
    )
  
}

regional_rivers_land_use <- map_dfr(seq(1, length(regions)), get_regional_rivers)

regional_rivers_land_use_total <- 
  regional_rivers_land_use %>%
    filter(`Land use category` != "Total") %>% 
    group_by(Year, Geography, `Land use category`) %>% 
    summarise(Value = sum(Value), `Water quality category` = "Total")


# combine new data -----------------------------------------------------------


new_rivers <- 
  bind_rows(
    rivers_land_use, rivers_land_use_total,
    regional_rivers, regional_rivers_total,
    regional_rivers_land_use, regional_rivers_land_use_total
  )


# combine with old river data ---------------------------------------------

# data from CIF
old_rivers <- 
  read_csv("C:/Users/pellmai/Documents/Open SDG/cif-data-donnees-cic/data/indicator_6-4-1.csv") %>% 
  mutate_all(~ str_remove(.x, "data.")) %>% 
  rename_all(~ str_remove(.x, "data.")) %>% 
  distinct() %>% 
  filter(
    Units == "Percentage",
    `Water quality` %in% c("Excellent", "Good", "Fair")
  ) %>% 
  rename(`Water quality category` = `Water quality`) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(-Units)
  
old_rivers <- 
  bind_rows(
    old_rivers,
    old_rivers %>% 
      group_by(Year, Geography, `Land use category`) %>% 
      summarise(Value = sum(Value), `Water quality category` = "Total")
  )
  
river_data <- 
  bind_rows(
    old_rivers,
    new_rivers
  )

write_csv(river_data, "rivers.csv")
