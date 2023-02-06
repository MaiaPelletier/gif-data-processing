# Translations -------------------------

library(cansim)
library(dplyr)
library(stringr)
library(readr)

en <- get_cansim("14-10-0417-01", factors = FALSE, language = "en")
fr <- get_cansim("14-10-0417-01", factors = FALSE, language = "fr")

dfs <- list(en, fr)

names(en)

filtering_dfs <- function(df) {
  
  df %>%
    # Need to change these selections to the disaggregations to be translated
    # and the respective hierarchy column
    select(c(6, 27)) %>%
    distinct() %>%
    # filter_at(c(2), ~ str_starts(.x, "1.2.3.34")) %>%
    # mutate_at(c(1), ~ str_remove(.x, " \\[.*\\]")) %>%
    rename_at(c(2), ~ "key")
    
  
}

disagg_names <- purrr::map(dfs, filtering_dfs)

disagg_names[[1]] %>% colnames()

translation <- 
  left_join(disagg_names[[1]], disagg_names[[2]]) %>%
  select(-key) %>%
  rename_at(c(1,2), ~ c("var1", "var2"))

en_translation <-
  translation %>% 
  transmute(
    Translation = paste0(var1, ": ", var1)
  ) %>%
  pull()

fr_translation <-
  translation %>% 
  transmute(
    Translation = paste0(var1, ": ", var2)
  ) %>%
  pull()

write_lines(en_translation, "en_translation.txt")            
write_lines(fr_translation, "fr_translation.txt")            
