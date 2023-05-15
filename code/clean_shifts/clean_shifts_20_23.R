## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-08
##

library(tidyverse)
library(readxl)

files <- list.files("raw_data/shifts_20-23/")

read_shifts <- function(file) {
  sheets <- excel_sheets(file)
  final_data <- map_df(sheets, ~read_xlsx(file, sheet = . )) %>% 
    janitor::clean_names()
  return(final_data)
}

shifts_1 <- read_shifts(paste0("raw_data/shifts_20-23/",files[1]))
shifts_2 <- read_shifts(paste0("raw_data/shifts_20-23/",files[2]))
shifts_4 <- read_shifts(paste0("raw_data/shifts_20-23/",files[4]))
shifts_5 <- read_shifts(paste0("raw_data/shifts_20-23/",files[5]))
shifts_6 <- read_shifts(paste0("raw_data/shifts_20-23/",files[6]))
shifts_7 <- read_shifts(paste0("raw_data/shifts_20-23/",files[7]))
shifts_10 <- read_shifts(paste0("raw_data/shifts_20-23/",files[10]))



# functions to clean ------------------------------------------------------

clean_shifts <- . %>% 
  janitor::clean_names() %>% 
  select(aa_date, unit, watch, name, title, star, present_for_duty,
         start_time, end_time, sworn) %>% 
  filter(sworn == "Y") %>% 
  filter(present_for_duty == "Y") %>% 
  distinct()

clean_shifts_2 <- . %>% 
  janitor::clean_names() %>% 
  select(date, unit, watch, first_name, last_name, title, star, present_for_duty,
         start_time, end_time, sworn) %>% 
  filter(sworn == "Y") %>% 
  filter(present_for_duty == "Y") %>% 
  distinct()


# file 3 is bad -----------------------------------------------------------

sheet_3_0 <- read_xlsx(paste0("raw_data/shifts_20-23/", files[3]),
                       sheet = "Export Worksheet")
sheet_3_1 <- read_xlsx(paste0("raw_data/shifts_20-23/", files[3]),
                       sheet = "Sheet1", col_names = colnames(sheet_3_0))
sheet_3_0 <- sheet_3_0 %>% 
  clean_shifts_2()
sheet_3_1 <- sheet_3_1 %>% 
  clean_shifts_2()
shifts_3 <- bind_rows(sheet_3_0, sheet_3_1)

# file 8 is bad -----------------------------------------------------------

sheet_8_0 <- read_xlsx(paste0("raw_data/shifts_20-23/",files[[8]]),
                       sheet = "Export Worksheet")

sheet_8_1 <- read_xlsx(paste0("raw_data/shifts_20-23/",files[[8]]),
                       sheet = "Sheet1", col_names = colnames(sheet_8_0))

sheet_8_2 <- read_xlsx(paste0("raw_data/shifts_20-23/",files[[8]]),
                       sheet = "Sheet2", col_names = colnames(sheet_8_0))

sheet_8_0 <- sheet_8_0 %>% 
  janitor::clean_names() %>% 
  mutate(date = dmy(date)) %>% 
  clean_shifts_2()
sheet_8_1 <- sheet_8_1 %>% 
  janitor::clean_names() %>% 
  mutate(date = dmy(date)) %>% 
  clean_shifts_2()

sheet_8_2 <- sheet_8_2 %>% 
  janitor::clean_names() %>% 
  mutate(date = dmy(date)) %>% 
  clean_shifts_2()

shifts_8 <- sheet_8_0 %>% bind_rows(sheet_8_1, sheet_8_2)

# file 9 is bad -----------------------------------------------------------

sheet_9_0 <- read_xlsx(paste0("raw_data/shifts_20-23/", files[9]),
                       sheet = "Data")
sheet_9_1 <- read_xlsx(paste0("raw_data/shifts_20-23/", files[9]),
                       sheet = "Sheet1", col_names = colnames(sheet_9_0))
sheet_9_2 <- read_xlsx(paste0("raw_data/shifts_20-23/", files[9]),
                       sheet = "Sheet2", col_names = colnames(sheet_9_0))

sheet_9_0 <- sheet_9_0 %>% clean_shifts_2()

sheet_9_1 <- sheet_9_1 %>% clean_shifts_2()

sheet_9_2 <- sheet_9_2 %>% clean_shifts_2()
shifts_9 <- bind_rows(sheet_9_0, sheet_9_1, sheet_9_2)

# end file 8 --------------------------------------------------------------



shifts_1 <- shifts_1 %>% 
  clean_shifts()

shifts_2 <- shifts_2 %>% clean_shifts()

shifts_3 <- shifts_3 %>% clean_shifts_2()

shifts_4 <- shifts_4 %>% clean_shifts()

shifts_5 <- shifts_5 %>% clean_shifts()

shifts_6 <- shifts_6 %>% clean_shifts_2()

shifts_7 <- shifts_7 %>% clean_shifts_2()

shifts_10 <- shifts_10 %>% clean_shifts()

shifts_all <- shifts_1 %>% 
  bind_rows(shifts_2, shifts_3, shifts_4,
            shifts_5, shifts_6, shifts_7,
            shifts_8, shifts_9, shifts_10)



shifts_all <- shifts_all %>% 
  mutate(aa_date = if_else(is.na(aa_date), date, aa_date)) 

shifts_all <- shifts_all %>% 
  mutate(aa_date = as_date(aa_date))




shifts_all <- shifts_all %>% 
  filter(sworn == "Y") %>% 
  filter(present_for_duty == "Y") %>% 
  distinct()



shifts_all %>% 
  write_csv("created_data/shifts_20_23.csv")
