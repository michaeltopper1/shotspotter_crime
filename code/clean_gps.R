## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-15
##

library(tidyverse)
library(dtplyr)

sheets <- readxl::excel_sheets("raw_data/gps_data/18524-P733257-In-Car-Camera-Data.xlsx")

in_car <- map_df(sheets, ~readxl::read_xlsx("raw_data/gps_data/18524-P733257-In-Car-Camera-Data.xlsx",
                                                   sheet = .))
in_car <- in_car %>% janitor::clean_names()

in_car <- in_car %>% 
  mutate(across(ends_with("date"), ~dmy_hms(.)),
         year = year(start_date),
         month = month(start_date))

in_car_2 <- lazy_dt(in_car)


in_car_small %>% 
  write_csv("created_data/car_camera.csv")
