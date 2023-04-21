library(tidyverse)

files <- list.files("raw_data/911_data/")

nineone <- map_df(files, ~read_csv(paste0("raw_data/911_data/",.)))

nineone <- nineone %>% 
  janitor::clean_names() %>% 
  mutate(entry_date = mdy_hms(entry_date))


nineone %>% 
  write_csv("created_data/911_toshio_17_21.csv")
