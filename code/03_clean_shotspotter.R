## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-24
##

library(tidyverse)

shotspotter <- read_csv("raw_data/Violence_Reduction_-_Shotspotter_Alerts.csv")

shotspotter <- shotspotter %>% 
  janitor::clean_names()

shotspotter <- shotspotter %>% 
  mutate(date = mdy_hms(date)) %>% 
  mutate(shotspot_year = year(date),
         shotspot_month = month(date),
         shotspot_year_month = mdy(paste0(shotspot_month, "-1-", shotspot_year)))

# shotspotter %>% 
#   write_csv("analysis_data/shotspotter_cleaned.csv")
# shotspotter %>% 
#   group_by(district, shotspot_year_month) %>% 
#   summarize(n = n()) %>% 
#   ggplot(aes(shotspot_year_month, n)) +
#   geom_line() +
#   facet_wrap(~district) +
#   theme_minimal()
