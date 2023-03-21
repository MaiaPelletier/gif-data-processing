
# 11.4.1 ------------------------------------------------------------------

library(cansim)
library(dplyr)
library(stringr)

ccofog <- get_cansim("10-10-0005-01", factors = FALSE)
# local <- get_cansim("10-10-0024-01", factors = FALSE)
pop <- get_cansim("17-10-0005-01", factors = FALSE)

selected_ccofog <- c(
  "Protection of biodiversity and landscape [7054]",
  "Cultural services [7082]",
  "Broadcasting and publishing services [7083]",
  "Recreation, culture, and religion not elsewhere classified [7084, 7085, 7086]"
)

national_provincial <-
  ccofog %>% 
  filter(
    REF_DATE >= 2015,
    `Canadian Classification of Functions of Government (CCOFOG)` %in% selected_ccofog
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
     `Level of government` = `Public sector components`,
    `Type of heritage` = 5,
    Expenditure = VALUE
  )

pop <- 
  pop %>% 
  filter(
    REF_DATE >= 2015,
    Sex == "Both sexes",
    `Age group` == "All ages"
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Population = VALUE
  )

# total expenditure on heritage
heritage_exp <- 
  national_provincial %>% 
  mutate(
    `Type of heritage` = str_remove(`Type of heritage`, " \\[.*\\]")
  ) %>% 
  bind_rows(
    national_provincial %>% 
    group_by(Year, Geography, `Level of government`) %>% 
    summarise(
      `Type of heritage` = "Total cultural and natural heritage", 
      Expenditure = sum(Expenditure, na.rm = TRUE),
      .groups = "drop"
    )
  ) %>% 
  left_join(pop) %>% 
  transmute(
    Year, Geography, `Level of government`, `Type of heritage`,
    Value = round(Expenditure/Population, 5)
  )


write.csv(
  heritage_exp,
  "data/indicator_11-4-1.csv",
  row.names = FALSE,
  na = ""
)

