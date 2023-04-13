## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-10
##



library(tidyverse)


filter_shifts <- function(shift_sheet) {
  filtered_shifts <- shift_sheet %>% 
    mutate(unit = parse_number(unit)) %>% 
    filter(unit < 26) %>% 
    filter(sworn != "N") %>% 
    filter(present_for_duty == "Y")
  return(filtered_shifts)
}

## note that this code takes a very long time to run. readxl::read_excel is very slow with large files

# 2016 --------------------------------------------------------------------


sheets <- readxl::excel_sheets("raw_data/A_A-2016-JAN-JUN.xlsx")

shifts_16_1 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2016-JAN-JUN.xlsx",
                                   sheet = .) %>% 
         janitor::clean_names() %>% 
         filter_shifts())


sheets <- readxl::excel_sheets("raw_data/A_A-2016-JUL-DEC.xlsx")

shifts_16_2 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2016-JUL-DEC.xlsx",
                                                  sheet = .) %>% 
                        janitor::clean_names() %>% 
                        filter_shifts())


# 2017 --------------------------------------------------------------------

sheets <- readxl::excel_sheets("raw_data/A_A-2017-JAN-JUN.xlsx")

sheets_17_1 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2017-JAN-JUN.xlsx",
                                                  sheet = .) %>% 
                        janitor::clean_names() %>% 
                        filter_shifts())

sheets <- readxl::excel_sheets("raw_data/A_A-2017-JUL-DEC.xlsx")

sheets_17_2 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2017-JUL-DEC.xlsx",
                                                  sheet = .) %>% 
                        janitor::clean_names() %>% 
                        filter_shifts())


# 2018 --------------------------------------------------------------------

sheets <- readxl::excel_sheets("raw_data/A_A-2018-JAN-JUN.xlsx")

sheets_18_1 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2018-JAN-JUN.xlsx",
                                                  sheet = .) %>% 
                        janitor::clean_names() %>% 
                        filter_shifts())

sheets <- readxl::excel_sheets("raw_data/A_A-2018-JUL-DEC.xlsx")

sheets_18_2 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2018-JUL-DEC.xlsx",
                                                  sheet = .) %>% 
                        janitor::clean_names() %>% 
                        filter_shifts())


# 2019 --------------------------------------------------------------------

sheets <- readxl::excel_sheets("raw_data/A_A-2019-JAN-JUN.xlsx")

sheets_19_1 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2019-JAN-JUN.xlsx",
                                                  sheet = .) %>% 
                        janitor::clean_names() %>% 
                        filter_shifts())

sheets <- readxl::excel_sheets("raw_data/A_A-2019-JUL-DEC.xlsx")

sheets_19_2 <- map_df(sheets, ~readxl::read_excel("raw_data/A_A-2019-JUL-DEC.xlsx",
                                                  sheet = .) %>% 
                        janitor::clean_names() %>% 
                        filter_shifts())

shifts_all <- bind_rows(shifts_16_1, shifts_16_2,
                        sheets_17_1, sheets_17_2,
                        sheets_18_1, sheets_18_2,
                        sheets_19_1, sheets_19_2)


shifts_all %>% 
  write_csv("created_data/shifts_appended.csv")
# beats_tosh %>% 
#   mutate(beat_zero = paste0("0", beat_no)) %>% 
#   extract(beat_zero, "beat", "(\\d{4,5})", remove = F) %>% 
#   mutate(beat_trunc = str_sub(beat,start = -4)) %>% 
#   count(beat_trunc) %>% View()
