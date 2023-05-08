

complaint <- haven::read_dta("/Users/michaeltopper/Downloads/complaints_cleaned_byofficer.dta")

rollout_dates <- read_csv("created_data/rollout_dates.csv")


complaint <- complaint %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(paste0(month,"-" ,day,"-", year)))


complaint <- complaint %>% 
  mutate(complainant_type = str_to_lower(complainant_type)) %>% 
  mutate(civilian_complaint = ifelse(complainant_type == "civilian", 1, 0))
complaint %>% 
  arrange(desc(date))

## creating panels for each of the districts
panel_dates <- seq(as_date("2016-01-01"), as_date("2020-07-22") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)

complaint <- complaint %>% 
  filter(year > 2015) %>%
  group_by(date, district) %>% 
  summarize(number_complaints = n(),
            number_civilian_complaints = sum(civilian_complaint, na.rm = T)) 

complaints_panel <- panel_dates %>% 
  left_join(complaint, join_by(district == district, date == date)) %>% 
  mutate(across(starts_with("number"), ~ifelse(is.na(.), 0, .)))



complaints_panel <- complaints_panel %>% 
  mutate(year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year))) %>% 
  left_join(rollout_dates, join_by(district == district))



complaints_panel <- complaints_panel %>% 
  mutate(treatment = ifelse(shotspot_activate >= date, 1, 0), .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment)) 

complaints_panel %>% 
  mutate(never_treated = ifelse(!district %in% rollout_dates$district, 1, 0)) %>% 
  fepois(number_civilian_complaints ~treatment | district^month^year ,
        cluster =~district)

rollout_dates
complaints_panel %>% 
  group_by(year_month, district, shotspot_activate) %>% 
  summarize(number_complaints = sum(number_complaints)) %>% 
  ggplot(aes(year_month, number_complaints)) +
  geom_line() +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "red") +
  facet_wrap(~district)



         