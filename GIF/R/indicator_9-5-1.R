# Indicator 9.5.1 ------------------------------------------------------
# 9.5.1 Research and development expenditure as a proportion of GDP

library(cansim)
library(dplyr)

rd_gdp <- get_cansim("27-10-0273-01", factors = FALSE)
annual_gdp <- get_cansim("36-10-0222-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

table_filter <- function(table) {
  
  table %>%
    filter(
      REF_DATE >= 2015,
      Prices == "Current prices"
    )
  
}


rd_gdp_data <- 
  left_join(
  # Research & dev gdp data
  rd_gdp %>%
    table_filter() %>%
    filter(
      Funder == "Funder: total, all sectors",
      Performer == "Performer: total, all sectors"
    ) %>%
    select(
      Year = REF_DATE,
      Geography = GEO,
      `Science type`,
      rd_gdp = VALUE
    ),
  # Annual gdp data
  annual_gdp %>% 
    table_filter() %>% 
    filter(Estimates == "Gross domestic product at market prices") %>% 
    select(
      Year = REF_DATE,
      Geography = GEO,
      annual_gdp = VALUE
    ),
  by = c("Year", "Geography")
  ) %>%
  transmute(
    Year, Geography, `Science type`,
    Value = round((rd_gdp/annual_gdp)*100, 2)
  ) %>% 
  left_join(geocodes) %>%
  mutate(GeoCode = ifelse(`Science type` == "Total sciences", GeoCode, NA)) %>%
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    rd_gdp_data %>%
      filter(Geography == "Canada", `Science type` == "Total sciences") %>%
      mutate(across(2:(ncol(.)-2), ~ "")),
    rd_gdp_data %>%
      filter(!(Geography == "Canada" & `Science type` == "Total sciences"))
  )  

write.csv(data_final, "data/indicator_9-5-1.csv", na = "", row.names = FALSE)

