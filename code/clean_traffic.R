## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-24
##

library(tidyverse)

sheets <- readxl::excel_sheets("raw_data/traffic_stops.xlsx")[-1]

traffic <- map_df(sheets, ~readxl::read_excel("raw_data/traffic_stops.xlsx",
                                             sheet = .,
                                             col_types = c(rep("guess", 1), 
                                                           rep("text", 12))) %>% 
                    janitor::clean_names() %>% 
                    mutate(datetime = dmy_hm(datetime)))
          
## drops only one observation        
traffic <- traffic %>% 
  distinct()

traffic_s <- traffic %>% 
  head(1000)

traffic <- traffic %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         year_month = mdy(paste0(month, "-1-", year)))

traffic <- traffic %>% 
  mutate(stopped_citizen_race = fct_lump(stopped_citizen_race, 4) %>% 
           str_to_lower()) 

traffic <- traffic %>% 
  mutate(stop_district = as.double(stop_district))

traffic <- traffic %>% 
  mutate(across(where(is.character), ~ . %>% str_to_lower()))

traffic <- traffic %>% 
  mutate(weapon_found_binary = ifelse(!is.na(weapon_found), 1, 0))

traffic <- traffic %>% 
  fastDummies::dummy_cols(select_columns = 
                            c("stopped_citizen_sex",
                              "stopped_citizen_race",
                              "search_conducted",
                              "disposition")) 
  
traffic <- traffic %>% 
  mutate(traffic_stop = 1)

traffic %>% 
  write_csv("created_data/traffic_stops_cleaned.csv")



## trends
# traffic %>% 
#   group_by(year_month) %>% 
#   summarize(stops = n()) %>% 
#   ggplot(aes(year_month, stops)) +
#   geom_line() +
#   theme_minimal()
# 
# 
#   

### sgt. Onesto