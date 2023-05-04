dispatches <- read_csv("created_data/dispatches_16_20.csv")


dispatches %>% 
  mutate(time_to_dispatch = first_dispatch_date - entry_date, .before = 1) %>% 
  mutate(time_to_dispatch = time_to_dispatch %>% 
           seconds_to_period() %>% 
           second()) %>% 
  ggplot(aes(time_to_dispatch)) +
  geom_histogram()
  
dispatches %>% 
  count(event_type, sort = T) %>% View()
nine <- read_csv("created_data/911_16_20_cleaned.csv")
dispatches %>% 
  filter(event_type == "SHOTSF") %>% 
  mutate(time_to_dispatch = first_dispatch_date - entry_date, .before = 1) %>% 
  mutate(time_to_dispatch = time_to_dispatch %>% 
           seconds_to_period() %>% 
           second()) %>% 
  ggplot(aes(time_to_dispatch)) +
  geom_histogram()
  dispatches_2 <- read_csv("created_data/dispatches.csv")
dispatches_2 %>% 
  count()