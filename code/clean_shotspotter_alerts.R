## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-12
##

library(tidyverse)

shotspotter <- read_csv("raw_data/shotspotter_alerts.csv") %>% 
  janitor::clean_names()

shotspotter <- shotspotter %>% 
  mutate(datetime = mdy_hms(date),
         date = as_date(datetime))

shotspotter <- shotspotter %>% 
  mutate(multiple_gunshots = ifelse(incident_type_description == "MULTIPLE GUNSHOTS", 1, 0)) 

shotspotter <- shotspotter %>% 
  mutate(year= year(date),
         month = month(date))

shotspotter %>%
  write_csv("created_data/shotspotter_cleaned.csv")


# shotspotter %>% 
#   group_by(district, shotspot_year_month) %>% 
#   summarize(n = n()) %>% 
#   ggplot(aes(shotspot_year_month, n)) +
#   geom_line() +
#   facet_wrap(~district) +
#   theme_minimal()
