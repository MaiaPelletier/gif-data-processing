
# 9.b.1 -------------------------------------------------------------------

# load libraries
library(dplyr)
library(cansim)
library(tidyr)

# load CODR table from stc api
Raw_data <- get_cansim("16-10-0117-01", factors = FALSE)

# load geocode
geocodes <- read.csv("geocodes.csv")

selected_naics <- c(
  "Manufacturing [31-33]",
  "Chemical manufacturing [325]",
  "Machinery manufacturing [333]",
  "Computer and electronic product manufacturing [334]",
  "Electrical equipment, appliance and component manufacturing [335]",
  "Transportation equipment manufacturing [336]",
  "Medical equipment and supplies manufacturing [3391]"
)

data_final <- 
  Raw_data %>% 
  filter(
    REF_DATE >= 2015,
    `Principal statistics` == "Manufacturing value added",
    `North American Industry Classification System (NAICS)` %in% selected_naics
  ) %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    NAICS = `North American Industry Classification System (NAICS)`,
    Value = VALUE
  ) %>% 
  mutate(
    NAICS = ifelse(NAICS == "Manufacturing [31-33]", "Total_MVA", "Tech_Industry"),
    Geography = ifelse(Geography == "Canada", "", Geography)
  ) %>% 
  group_by(Year, Geography, NAICS) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  pivot_wider(
    names_from = "NAICS",
    values_from = "Value"
  ) %>% 
  transmute(
    Value = round((Tech_Industry/Total_MVA)*100, 2)
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = "Value")

write.csv(
  data_final,
  "data/indicator_9-b-1.csv",
  row.names = FALSE,
  na = ""
)
