library(tidyverse)


## there needs to be analysis filters on certain types
## of calls. This is in the docx file in the raw_data/cincinnati folder 
cin <- read_csv("raw_data/cincinnati/PDI__Police_Data_Initiative__Police_Calls_for_Service__CAD__20240216.csv")

cin <- cin %>% 
  janitor::clean_names()


cin <- cin %>% 
  mutate(across(c(create_time_incident,arrival_time_primary_unit,
                  closed_time_incident, dispatch_time_primary_unit), 
                ~mdy_hms(.)))

cin <- cin %>% 
  mutate(date = as_date(create_time_incident),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

sst_cin <- cin %>% 
  filter(incident_type_id == "SPOTS")

sst_cin %>% 
  group_by(district, year_month) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(year_month, n, color = as.factor(district))) +
  geom_path() +
  facet_wrap(~district)

cin %>% 
  count(priority_color, priority, sort = T) %>% View()
## district 1: implementation looks liek 2020-05-18
## district 2: implementation looks like 2020-06-15
## district 3: implementation looks like 2019-07-15
## district 4: implementation looks like 2017-08-16
## district 5: implementation looksl ike 2020-06-14
sst_cin %>% 
  group_by(district) %>% 
  slice_max(order_by = desc(create_time_incident), n = 20) %>% 
  filter(district == 5) 


cin %>% 
  count(priority, incident_type_id, priority_color, sort = T) %>% View()
