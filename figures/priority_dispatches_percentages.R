## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-08
##

library(tidyverse)

dispatches <- read_csv("created_data/dispatches_appended_16_23.csv")

dispatches <- dispatches %>% 
  mutate(priority = str_extract(priority,"^.")) 


top_ten <- dispatches %>% 
  mutate(year = year(entry_date)) %>% 
  filter(priority == "1") %>% 
  filter(year > 2019) %>% 
  count(event_type, sort = T) %>% 
  mutate(total = sum(n),
         percentage = n/total) %>% 
  head(10)


## this is only 2019 and beyond
## beforehand, the data is a little iffy. But this is to once all sst has been implemented
top_ten %>% 
  mutate(event_type = fct_reorder(event_type, n)) %>% 
  mutate(percentage = percentage * 100) %>% 
  mutate(percentage = sprintf(percentage,fmt =  "%.1f")) %>% 
  mutate(percentage = glue::glue("{percentage}%")) %>% 
  mutate(shotspot = ifelse(event_type == "SST", "ShotSpotter", NA)) %>% 
  ggplot(aes(event_type, n, fill = shotspot)) +
  geom_col() +
  geom_text(aes(label = percentage), hjust = -.04) +
  scale_y_continuous(label = scales::comma) +
  coord_flip() +
  theme_minimal() +
  expand_limits(y = c(0, 400000)) +
  labs(y = "Number of Priority 1 Dispatches (2019-2023)", x = "Event Type") +
  theme(legend.position = "non")

dispatches <- dispatches %>% 
  mutate(year = year(entry_date))

dispatches <- dispatches %>% 
  mutate(priority = parse_number(priority))


  
