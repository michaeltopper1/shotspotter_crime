

sst_dispatches <- read_csv("created_data/sst_dispatch_cpd.csv")


## number of sst alerts within the hour on a date within a beat
hourly_sst <- sst_dispatches %>% 
  mutate(date = as_date(entry_received_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)),
          hour = hour(date)) %>% 
  count(district,beat_of_occurrence, hour, date, sort = T) 

hourly_sst <- hourly_sst %>% 
  mutate(district = parse_number(district))

hourly_sst <- hourly_sst %>% 
  mutate(hour_before = if_else(hour == 0, 23, hour - 1),
         hour_after = if_else(hour == 23, 0, hour + 1))

hourly_sst_before <- hourly_sst %>% 
  select(-hour, -hour_after) %>% 
  rename(sst_after_count = n)

hourly_sst_after <- hourly_sst %>% 
  select(-hour, -hour_before) %>% 
  rename(sst_before_count = n)


## number of SST alerts within a beat within an hour
  
## variation is the number of alerts within the hour
## indicator is 1 if SST alert within the hour in the beat.

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  left_join(hourly_sst)

dispatch_panel_p1 %>% 
  mutate(n = replace_na(n, 0)) %>% 
  mutate(hourly_sst = if_else(n > 0, 1, 0)) %>% 
  filter(final_dispatch_description == "SHOTS FIRED") %>% 
  feols(entry_to_dispatch ~ treatment | district + date +
          final_dispatch_description + hour) %>% 
  fitstat(type = "my")

