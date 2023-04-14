## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-13
##

library(tidyverse)

codebook <- readxl::read_excel("raw_data/chicago_crime_codebook.xlsx")

codebook <- codebook %>% 
  extract(iucr, into = c("iucr_code", "iucr_description"),
          "(.{4})\\s(.{1,})",
          remove = F)

codebook <- codebook %>% 
  mutate(nibrs = str_replace_all(nibrs,"^\\(.{1,}", "")) %>% 
  extract(nibrs, into = "fbi_code", "\\((.{2,3})\\)",
          remove = F) %>% 
  fill(fbi_code) 

codebook <- codebook %>% 
  mutate(across(iucr_description, ~str_to_lower(.))) %>% 
  mutate(gun_involved = ifelse(str_detect(iucr_description,"gun|firearm"),1,0 )) %>% 
  drop_na(iucr_code) %>% 
  select(-nibrs)

codebook <- codebook %>% 
  mutate(unlawful_sale_gun = ifelse(str_detect(iucr_description, "unlawful sale") & gun_involved == 1, 1, 0))

codebook %>% 
  write_csv("created_data/crime_codebook_cleaned.csv")
