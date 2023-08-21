## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-08-15
##

library(tidyverse)

victim_define <- read_csv("created_data/victims_descriptions.csv")

options(knitr.kable.NA = '')

footnotes <- map(list( "All descriptions are from confirmed 911 dispatches that resulted
                       in an injury. Time-Insensitive 911 dispatches are those in which
                       an injury has likely been realized prior to an officer dispatch.
                       On the other hand, Time-Sensitive dispatches are those in which
                       a victim may avoid injury if an officer can rapidly respond in time
                       to intervene. 
                  "), ~str_remove_all(., "\n"))

gun_related <- tibble("Gun-Related"= c("PERSON WITH A GUN", "SHOTS FIRED",
                                       rep(" ", 36)))
injury_descriptions <- victim_define %>% 
  select(final_dispatch_description, injury_possibility) %>% 
  distinct(final_dispatch_description, .keep_all = T) %>% 
  group_by(injury_possibility) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = injury_possibility,
              values_from = final_dispatch_description,
              id_cols = id) %>% 
  bind_cols(gun_related,) %>% 
  rename("Injury Realized" = `0`,
         "Potential for Injury" = `1`) %>% 
  select(-id) %>% 
  kbl(caption = "\\label{injury_descriptions}Categorization for Injury-Related Dispatches",
      booktabs = T) %>% 
  add_header_above(c("Injury-Confirmed Call Descriptions" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 9)
