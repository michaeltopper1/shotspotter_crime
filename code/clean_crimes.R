## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-07
##

library(tidyverse)

files <- list.files(path = "raw_data/", pattern = "^crime")

crimes <- map_df(files, ~read_csv(paste0("raw_data/", .)))

crimes <- crimes %>% janitor::clean_names() 

crimes %>% 
  count(primary_type, sort = T) %>% View()

crimes <- crimes %>% 
  mutate(district = parse_number(district))

crimes <- crimes %>% 
  mutate(datetime = mdy_hms(date),
         date = as_date(datetime),
         time = hms::as_hms(datetime),
         year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year)))

crimes <- crimes %>% 
  mutate(arrest = ifelse(arrest == T, 1, 0),
         domestic = ifelse(domestic == T, 1, 0)) 



crimes %>% 
  write_csv("created_data/crimes_cleaned.csv")
  



# crimes %>% 
#   group_by(year_month, primary_type, district) %>% 
#   summarize(number_types = n()) %>% 
#   filter(primary_type == "CONCEALED CARRY LICENSE VIOLATION" | primary_type == "WEAPONS VIOLATION") %>% 
#   ggplot(aes(year_month, number_types)) +
#   geom_line() +
#   geom_smooth(method = "lm") +
#   facet_wrap(~district) +
#   theme_minimal()
#   
