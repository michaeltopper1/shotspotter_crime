## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-20
##

library(tidyverse)

## chicago tribune data from 16-20
dispatch_16_20 <- read_csv("created_data/dispatches_16_20.csv")
## my own data from 19-23
dispatch_20_23 <- read_csv("created_data/dispatches.csv")
## toshio has data from 17 to 21
toshio <- read_csv("created_data/911_toshio_17_21.csv") %>% 
  select(event_number, disposition) %>% 
  rename(event_type = disposition) %>% 
  distinct()

dispatch_20_23 <- dispatch_20_23 %>%
  left_join(toshio)

## filtering to below when my data is created
dispatch_16_20 <- dispatch_16_20 %>% 
  mutate(tribune_data = 1) %>% 
  filter(entry_date < as_date("2019-03-01"))



all_dispatches <- dispatch_20_23 %>% 
  mutate(tribune_data = 0) %>% 
  bind_rows(dispatch_16_20) %>% 
  arrange(entry_date)


rm(dispatch_16_20, toshio, dispatch_20_23)

all_dispatches %>% 
  write_csv("created_data/dispatches_appended_16_23.csv")
