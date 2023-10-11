library(tidyverse)


shifts_1 <- read_csv("created_data/shifts_appended.csv")
shifts_2 <- read_csv("created_data/shifts_20_23.csv")

overtime <- readxl::read_excel("raw_data/P847446-Overtime-Data-2014-Apr2023.xlsx") %>% 
  janitor::clean_names()




# getting shifts in -------------------------------------------------------

## this will make certain that I can filter to only police officers within the overtime data

shifts_all <- shifts_1 %>% 
  mutate(unit = as.character(unit)) %>% 
  bind_rows(shifts_2)

shifts_all <- shifts_all %>% 
  mutate(police_officer = ifelse(title == "POLICE OFFICER", 1, 0)) %>% 
  distinct()



# cleaning the overtime data ----------------------------------------------

overtime <- overtime %>% janitor::clean_names()

overtime <- overtime %>% 
  mutate(date = dmy(overtime_date))


## restricting to districts that are in the data and have patrol units
overtime <- overtime %>% 
  mutate(district = parse_number(unit_at_ot)) %>% 
  filter(district %in% c(1:25))

overtime <- overtime %>% 
  mutate(date = as_date(date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year))) %>% 
  filter(year < 2023 & year > 2015)

overtime <- overtime %>% 
  mutate(duty_hour_from = dmy_hm(duty_hour_from),
         duty_hour_to = dmy_hm(duty_hour_to)) 

## necessary for filtering later on to get only police officers
overtime <- overtime %>% 
  mutate(name = glue::glue("{first_name} {middle_initial} {last_name}"))
  
## getting rid of any over time that doesn't make sense
## where the end hour is greater than the starting hour
overtime <- overtime %>% 
  filter(!duty_hour_from > duty_hour_to) 


overtime <- overtime %>% 
  mutate(start_date = as_date(duty_hour_from),
         end_date = as_date(duty_hour_to))

overtime <- overtime %>% 
  mutate(different_end_date = if_else(start_date != end_date, 1, 0)) 



# two different data sets for those that start before ---------------------
# and after the dates
overtime_same_day <- overtime %>% 
  filter(different_end_date != 1)
overtime_different_day <- overtime %>% 
  filter(different_end_date == 1)


overtime_same_day <- overtime_same_day %>% 
  mutate(overtime_1 = time_length(duty_hour_to - duty_hour_from, "hours"),
         .before = 1) %>% 
  mutate(date = start_date)



# need to run the create_panel_analysis_shifts_officers.R for this to run ----------------------------

overtime_same_day <- overtime_same_day %>% 
  filter(star_no %in% shifts_all$star | name %in% shifts_all$name)


## create two columns: 
## one for start date hours 
## one for end date hours

overtime_different_day <- overtime_different_day %>% 
  relocate(duty_hour_from, duty_hour_to) %>% 
  mutate(start_time = hms::as_hms(duty_hour_from),
         end_time = hms::as_hms(duty_hour_to),
         midnight_0 = hms::as_hms(strptime("24:00", "%H:%M")),
         midnight_24 = hms::as_hms(strptime("23:59", "%H:%M")), .before = 1) %>% 
  mutate(first_day_hours = time_length(midnight_24 - start_time, "hours"),
         second_day_hours = time_length(end_time - midnight_0, "hours"), .before = 1) 


overtime_different_day_half_1 <- overtime_different_day %>% 
  mutate(overtime_2 = first_day_hours,
         date = start_date) %>% 
  select(-second_day_hours) 

overtime_different_day_half_1 <- overtime_different_day_half_1 %>% 
  filter(star_no %in% shifts_all$star | name %in% shifts_all$name)

# need to run the create_panel_analysis_shifts_officers first -------------

overtime_different_day_half_2 <- overtime_different_day %>% 
  mutate(overtime_3 = second_day_hours,
         date = end_date) %>% 
  select(-first_day_hours) 

overtime_different_day_half_2 <- overtime_different_day_half_2 %>% 
  filter(star_no %in% shifts_all$star | name %in% shifts_all$name)




# aggregating -------------------------------------------------------------

overtime_same_day <- overtime_same_day %>% 
  group_by(date, district) %>% 
  summarize(overtime_1 = sum(overtime_1, na.rm = T)) %>% 
  ungroup()


overtime_different_day_half_1 <- overtime_different_day_half_1 %>% 
  group_by(date, district) %>% 
  summarize(overtime_2 = sum(overtime_2, na.rm = T)) %>% 
  ungroup()


overtime_different_day_half_2 <- overtime_different_day_half_2 %>% 
  group_by(date, district) %>% 
  summarize(overtime_3 = sum(overtime_3, na.rm = T)) %>% 
  ungroup()



  

# creating a panel --------------------------------------------------------

panel_dates <- seq(as_date("2016-01-01"), as_date("2022-12-31") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)


overtime_officer_hours <- panel_dates %>% 
  left_join(overtime_same_day) %>% 
  left_join(overtime_different_day_half_1) %>% 
  left_join(overtime_different_day_half_2) %>% 
  mutate(across(starts_with("overtime"), ~if_else(is.na(.), 0, .)))


overtime_officer_hours <- overtime_officer_hours %>% 
  mutate(overtime_hours = overtime_1 + overtime_2 + overtime_3) 

overtime_officer_hours %>% 
  write_csv("created_data/overtime_hours.csv")
