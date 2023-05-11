## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-09
##

library(tidyverse)

shifts_1 <- read_csv("created_data/shifts_appended.csv")
shifts_2 <- read_csv("created_data/shifts_20_23.csv")
panel <- read_csv("analysis_data/crimes_panel.csv")


shifts_all <- shifts_1 %>% 
  mutate(unit = as.character(unit)) %>% 
  bind_rows(shifts_2)

shifts_all <- shifts_all %>% 
  mutate(police_officer = ifelse(title == "POLICE OFFICER", 1, 0)) %>% 
  distinct()

shifts_all <- shifts_all %>% 
  mutate(unit = parse_number(unit)) %>% 
  filter(unit %in% panel$district)

shifts_all <- shifts_all %>% 
  mutate(year = year(aa_date),
         month = month(aa_date),
         year_month = mdy(paste0(month, "-1-", year)))


## getting rid of any duplicates on the same date/time
shifts_all <- shifts_all %>% 
  distinct(star, aa_date, start_time, .keep_all = T) 



## creating start time and end times for each shift
## start date hours 1 is the number of hours in a shift
## if the shift starts and ends on the same day
## I need to split the hours into separate days when they start/end on different days
shifts_all <- shifts_all %>% 
  mutate(start_time = strptime(start_time, format = "%H%M") %>% 
           hms::as_hms(),
         end_time = strptime(end_time, format = "%H%M") %>% 
           hms::as_hms(),
         shift_difference = time_length(end_time - start_time, unit = "seconds"),
         start_date = aa_date,
         end_date = if_else(shift_difference <= 0, start_date + days(1), start_date),.before = 1) %>% 
  relocate(start_date, end_date) %>% 
  mutate(start_date_hours_1 = if_else(start_date == end_date, 
                                 time_length(end_time - start_time, unit = "hours"),
                                 NA),
         .before = 1)

## need to create 2 columns
## 1 is endtime - 00:00 if enddate > start_date
## 2 is 24:00 - starttime if enddate > start_date
## These give me the hours for each chunk of when enddate > startdate
shifts_all <- shifts_all %>% 
  mutate(start_date_hours_2 = if_else(end_date > start_date, 
                                  time_length(strptime("24:00", "%H:%M") - strptime(start_time, "%H:%M"), unit = "hour"),
                                  NA), 
         end_date_hours = if_else(end_date > start_date,
                                  time_length(strptime(end_time, "%H:%M") - strptime("00:00", "%H:%M"), unit = "hour"),
                                  NA),
         .before = 1) 

## this is getting only the start_date hours together
start_date_hours <- shifts_all %>% 
  filter(police_officer == 1) %>%
  group_by(unit, start_date) %>% 
  summarize(start_date_hours_1 = sum(start_date_hours_1, na.rm = T),
            start_date_hours_2 = sum(start_date_hours_2, na.rm = T)) %>% ungroup()

## this is getting only the end_date hours together
end_date_hours <- shifts_all %>%
  filter(police_officer == 1) %>%
  group_by(unit, end_date) %>% 
  summarize(end_date_hours = sum(end_date_hours, na.rm = T, na.rm = T)) %>% 
  ungroup()


# creating panel ----------------------------------------------------------

## creating panels for each of the districts
## starts on sundays
panel_dates <- seq(as_date("2016-01-01"), as_date("2022-12-31") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)


# connecting to panel -----------------------------------------------------

## this joing the start date hours and end date hours.
## the last mutate adds them all up.
officer_hours_panel <- panel_dates %>% 
  left_join(start_date_hours, join_by(date == start_date,
                                      district == unit)) %>% 
  left_join(end_date_hours, join_by(date == end_date,
                                    district == unit)) %>% 
  mutate(officer_hours = start_date_hours_1 + start_date_hours_2 + end_date_hours)


officer_hours_panel <- officer_hours_panel %>% 
  mutate(year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year)))

officer_hours_panel %>% 
  write_csv("analysis_data/officer_hours.csv")


shifts_all %>% 
  filter(police_officer == 1) %>%
  count(year_month) %>% 
  ggplot(aes(year_month, n)) +
  geom_line() +
  labs(x = "", y = "Number of Police Officer Shifts") +
  theme_minimal()
shifts_all %>% View()

