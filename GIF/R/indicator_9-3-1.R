library(tidyverse)
library(readxl)
library(here)

bind_value_add_data <- function(sheet_no) {
  
  sheet <- read_excel(
    "GIF/R/non-codr/raw data/Copy of GIF 9.3.1 - Proportion of small-scale industries in total industry value added.xlsx",
    sheet = sheet_no
  )
  
  sheet %>% 
    # janitor::clean_names() %>% 
    select(-c(1, 3)) %>% 
    pivot_longer(
      cols = 3:8,
      names_to = "Size",
      values_to = "Value"
    ) %>% 
    mutate(
      Value = as.numeric(Value),
      Value = round(Value, 2)
    ) %>% 
    rename(Industry = INDUSTRY) %>% 
    relocate(Industry, .after = Year)
  
}

data <- purrr::map_dfr(4:5, bind_value_add_data)

total_manufacturing <- 
  data %>% 
  filter(
    Industry == "Total manufacturing",
    Size == "Total"
  ) %>% 
  select(Year, Total = Value)

data_final <- 
  data %>%
  left_join(total_manufacturing) %>% 
  mutate(Proportion = (Value/Total)*100) %>% 
  select(
    Year,
    Industry,
    Size,
    Value = Proportion
  ) %>% 
  filter(Value > 0, !is.na(Value)) %>% 
  filter(!(Industry == "Total manufacturing" & Size == "Total")) %>% 
  mutate(
    Size = case_when(
      Size == "Total" ~ "All enterprises",
      Size == "250 and more" ~ "250 employees and more",
      TRUE ~ paste0(Size, " employees")
    )
  )

write_csv(data_final, here("GIF", "data", "indicator_9-3-1.csv"), na = "")
  