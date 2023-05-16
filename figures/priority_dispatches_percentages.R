## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-08
##

library(tidyverse)

dispatches <- read_csv("analysis_data/dispatches_clevel.csv")


dispatches <- dispatches %>%   
  mutate(entry_to_dispatch = time_length(first_dispatch_date - entry_received_date, "seconds"),
                        entry_to_onscene = time_length(on_scene_date - entry_received_date, "seconds"),
                        dispatch_to_onscene = time_length(on_scene_date - first_dispatch_date, "seconds"),
                        entry_to_close = time_length(close_completed_date - entry_received_date, "seconds"))

top_ten <- dispatches %>% 
  # mutate(year = year(entry_date)) %>% 
  filter(priority_code == 1) %>% 
  filter(year > 2019) %>% 
  count(final_dispatch_description, sort = T) %>% 
  mutate(total = sum(n),
         percentage = n/total) %>% 
  head(10)


## this is only 2019 and beyond
## beforehand, the data is a little iffy. But this is to once all sst has been implemented
top_ten %>% 
  mutate(final_dispatch_description = fct_reorder(final_dispatch_description, n)) %>% 
  mutate(percentage = percentage * 100) %>% 
  mutate(percentage = sprintf(percentage,fmt =  "%.1f")) %>% 
  mutate(percentage = glue::glue("{percentage}%")) %>% 
  mutate(shotspot = ifelse(final_dispatch_description == "SHOT SPOTTER", "ShotSpotter", 0)) %>% 
  ggplot(aes(final_dispatch_description, n, fill = shotspot)) +
  geom_col() +
  geom_text(aes(label = percentage), hjust = -.04, size = 3) +
  scale_y_continuous(label = scales::comma) +
  coord_flip() +
  theme_minimal() +
  expand_limits(y = c(0, 400000)) +
  labs(y = "Number of Priority 1 Dispatches (2019-2023)", x = "Event Type") +
  theme(legend.position = "non") +
  ggthemes::scale_fill_stata()



dispatches %>% 
  filter(time_to_dispatch < 6000 & time_to_dispatch >= 0) %>% 
  ggplot(aes(time_to_dispatch)) +
  geom_histogram() +
  facet_wrap(~priority_code, scales = "free_y") +
  theme_minimal()

dispatches %>%
  mutate(dispatch_to_onscene = time_length(on_scene_date - first_dispatch_date, "minutes")) %>% 
  filter(dispatch_to_onscene <= 0) %>% 
  count(call_source_description, sort = T) %>% 
  mutate(sum(n)) %>% View()

dispatches %>% 
  filter(call_source_description == "E-911") %>% 
  count(year)
  count(call_source_description, sort = T)
dispatches %>% 
  filter(final_dispatch_description == "TRAFFIC STOP (OV)") %>% 
  count(priority_code)
