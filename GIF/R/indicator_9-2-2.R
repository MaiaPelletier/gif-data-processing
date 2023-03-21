# Indicator 9.2.2 ------------------------------------------------------
# Manufacturing employment as a proportion of total employment

library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

employment_data <- get_cansim("14-10-0202-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")


manufacturing_employment <-
  employment_data %>%
  filter(
    REF_DATE >= 2015,
    str_starts(
      `Hierarchy for North American Industry Classification System (NAICS)`, 
      "1.2.3.34"
      ) | `Hierarchy for North American Industry Classification System (NAICS)` == "1"
  ) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of employee`,
    Industry = `North American Industry Classification System (NAICS)`,
    Value = VALUE
  ) %>% distinct(Industry)
  mutate(
    Industry = str_remove(Industry, " \\[.*\\]"),
    Industry = ifelse(str_detect(Industry, "Industrial aggregate"), "All industries", Industry),
    Industry_type = ifelse(Industry == "All industries", "Total", "Manufacturing")
  ) %>%
  tidyr::pivot_wider(
    names_from = Industry_type,
    values_from = Value
  ) %>%
  tidyr::fill(Total) %>%
  filter(Industry != "All industries") %>%
  transmute(
    Year, Geography, `Type of employee`, Industry, 
    Value = round((Manufacturing/Total)*100, 2)
  ) %>%
  left_join(geocodes) %>%
  relocate(GeoCode, .before = Value) %>%
  mutate(GeoCode = ifelse(`Type of employee` == "All employees" & `Industry` == "Manufacturing", GeoCode, NA))


data_final <- 
  bind_rows(
    manufacturing_employment %>%
      filter(Geography == "Canada", `Type of employee` == "All employees", Industry == "Manufacturing") %>%
      mutate(across(2:(ncol(.)-2), ~ "")),
    manufacturing_employment %>%
      filter(!(Geography == "Canada" & `Type of employee` == "All employees" & Industry == "Manufacturing"))
  )

# write_csv(data_final, here("gif-data-processing", "data", "indicator_9-2-2.csv"), na = "")
