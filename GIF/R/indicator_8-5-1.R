# Indicator 8.5.1 ------------------------------------------------------
# Average hourly earnings of employees, by sex, age, occupation and persons with
# disabilities

library(dplyr)
library(cansim)
library(stringr)

earnings <- get_cansim("14-10-0417-01", factors = FALSE)
geocodes <- read.csv("geocodes.csv")

selected_occupations <- c(
  "Total employees, all occupations [00-95]",
  "Management occupations [00, 10, 20, 30, 40, 50, 60, 70, 80, 90]",
  "Business, finance and administration occupations, except management [11-14]",
  "Natural and applied sciences and related occupations, except management [21-22]",
  "Health occupations, except management [31-33]",
  "Occupations in education, law and social, community and government services, except management [41-45]",
  "Occupations in art, culture, recreation and sport, except management [51-55]",
  "Sales and service occupations, except management [62-65]",
  "Trades, transport and equipment operators and related occupations, except management [72-75]",
  "Natural resources, agriculture and related production occupations, except management [82-85]",
  "Occupations in manufacturing and utilities, except management [92-95]"
)


filtered_earnings <- 
 earnings %>% 
  filter(
    REF_DATE >= 2015,
    Wages == "Average hourly wage rate",
    `National Occupational Classification (NOC)` %in% selected_occupations
  )

all_earnings <- 
  filtered_earnings %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    `Type of work`,
    Occupation = `National Occupational Classification (NOC)`,
    Sex,
    `Age group`,
    Value = VALUE
  ) %>% 
  mutate(
    Occupation = str_remove(Occupation, " \\[.*\\]")
  ) %>% 
  left_join(geocodes) %>% 
  relocate(GeoCode, .before = Value)

data_final <- 
  bind_rows(
    # total line
    all_earnings %>%
      filter(
        Geography == "Canada",
        `Type of work` == "Both full- and part-time employees",
        Occupation == "Total employees, all occupations",
        Sex == "Both sexes",
        `Age group` == "15 years and over"
      ) %>% 
      mutate_at(2:6, ~ ""),
    
    # disaggregates
    all_earnings %>%
      filter(
        !(
          Geography == "Canada" &
            `Type of work` == "Both full- and part-time employees" &
            Occupation == "Total employees, all occupations" &
            Sex == "Both sexes" &
            `Age group` == "15 years and over"
        )
    )
  )


write.csv(
  data_final,
  "data/indicator_8-5-1.csv",
  na = "",
  row.names = FALSE
)
