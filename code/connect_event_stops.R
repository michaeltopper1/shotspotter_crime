

str_length("1906000302")

old_911 <- read_csv("created_data/911_16_20_cleaned.csv")
shotspotter_alerts <- read_csv("created_data/shotspotter_cleaned.csv")
stops %>% 
  mutate(event_no_count = str_length(event_no), .before = 1) %>% 
  filter(event_no %in% shotspotter_dispatches$event_number) %>% View()

shotspot_alerts_old <- old_911 %>% 
  filter(final_event_type == "SST") 

shotspot_alerts_old_filter <- shotspot_alerts_old %>% 
  filter(entry_date < as_date("2019-03-01"))

shotspot_alerts_new <- shotspotter_dispatches %>% 
  distinct(event_number, entry_date)

shotspot_alerts_event_numbers <- bind_rows(shotspot_alerts_old_filter,
                                           shotspot_alerts_new)


shotspot_alerts_old %>% 
  mutate(count  = str_length(event_number), .before = 1) %>% 
  count(count)
shotspot_alerts_event_numbers %>% View()

stops <- stops %>% 
  mutate(shotspot_stop = ifelse(event_no %in% shotspot_alerts_event_numbers$event_number,
                                1, 0)) 
  
shotspotter_alerts %>% 
  separate_wider_delim(cols = unique_id, 
                       delim = "-",
                       names = c("type", "number"),
                       too_many = "merge") %>% 
  count(type)
stops %>% 
  mutate(person_stopped_race = ifelse(is.na(person_stopped_race), "White", person_stopped_race)) %>% 
  group_by(shotspot_stop) %>% 
  count(person_stopped_race) %>% 
  mutate(total = sum(n, na.rm = T)) %>% 
  rowwise() %>% 
  mutate(percent = n/total)
