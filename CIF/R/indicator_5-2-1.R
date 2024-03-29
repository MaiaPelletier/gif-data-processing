# CIF 5.2.1 ---------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(stringr)

# load CODR table from stc api
repr_in_gov <- get_cansim("10-10-0137-01", factors = FALSE)
repr_in_mgmt <- get_cansim("14-10-0335-01", factors = FALSE) 
repr_in_judges <- get_cansim("37-10-0208-01", factors = FALSE)
repr_in_chiefs <- get_cansim("41-10-0048-01", factors = FALSE)


# Representation in government --------------------------------------------

repr_in_gov <- 
  repr_in_gov %>%
  mutate(
    Year = str_extract(`National elected officials`, "[0-9]{4}"),
    `Leadership position` = case_when(
      str_detect(`National elected officials`, "Parliament") ~ "Members of national Parliament",
      str_detect(`National elected officials`, "Cabinet") ~ "Members of federal Cabinet",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(
    Year >= 2002,
    GEO == "Canada",
    Gender == "Women",
    Statistics == "Proportion"
  ) %>% 
  select(
    Year,
    `Leadership position`,
    Value = VALUE
  )


# Representation in management --------------------------------------------

selected_occupations <- c(
  "Management occupations [0]",
  "Senior management occupations [00]",
  "Specialized middle management occupations [01-05]",
  "Middle management occupations in retail and wholesale trade and customer services [06]",
  "Middle management occupations in trades, transportation, production and utilities [07-09]"
)

repr_in_mgmt <- 
  repr_in_mgmt %>% 
  filter(
    REF_DATE >= 2002,
    GEO == "Canada",
    `Labour force characteristics` == "Proportion of employment", 
    `National Occupational Classification (NOC)` %in% selected_occupations,
    Sex == "Females"
  ) %>% 
  select(
    Year = REF_DATE,
    `Leadership position` = `National Occupational Classification (NOC)`,
    Value = VALUE
  ) %>% 
  mutate(
    `Leadership position` = str_remove_all(`Leadership position`, " \\[.*\\]"),
    `Leadership position` = ifelse(
      `Leadership position` == "Management occupations",
      "All management occupations",
      `Leadership position`
    )
  )


# Representation in judges ------------------------------------------------

selected_industries <- c(
  "Federal government public administration [911]",
  "Provincial and territorial public administration [912]"
)

repr_in_judges <- 
  repr_in_judges %>% 
  filter(
    REF_DATE >= 2002,
    GEO == "Canada",
    `North American Industry Classification System (NAICS)` %in% selected_industries,
    Sex == "Females",
    `Selected demographic characteristics` == "Total, all judges",
    Statistics == "Percentage of persons"
  ) %>% 
  select(
    Year = REF_DATE,
    `Leadership position` = `North American Industry Classification System (NAICS)`,
    Value = VALUE
  ) %>% 
  mutate(
    `Leadership position` = str_remove(`Leadership position`, " \\[.*\\]"),
    `Leadership position` = paste0("Judges - ", `Leadership position`)
  )


# Representation in chiefs and council ------------------------------------

repr_in_chiefs <-
  repr_in_chiefs %>%
  filter(
    REF_DATE >= 2002,
    Sex == "Female",
    Statistics == "Proportion"
  ) %>% 
  select(
    Year = REF_DATE,
    `Leadership position` = `First Nation Official`,
    Value = VALUE
  )
  
  

# Combine data ------------------------------------------------------------

data_final <-
  bind_rows(repr_in_gov, repr_in_mgmt, repr_in_judges, repr_in_chiefs) %>% 
  mutate_at(2, ~ paste0("data.", .x)) %>% 
  rename_at(2, ~ paste0("data.", .x))


write.csv(
  data_final,
  "data/indicator_5-2-1.csv",
  na = "",
  row.names = FALSE
)  
  
  
  