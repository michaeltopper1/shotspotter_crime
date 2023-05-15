library(tidyverse)
library(readxl)


## I'm only reading them in like this because 1) it takes forever
## and 2) map_df sometimes runs into errors if column types are not the same
## and 3) because there are multiple sheets within an excel file.
read_dispatches <- function(file) {
  sheets <- excel_sheets(file)
  final_data <- map_df(sheets, ~read_xlsx(file, sheet = . )) %>% 
    janitor::clean_names()
  return(final_data)
}


# reading in all data -----------------------------------------------------

dispatch_16 <- read_dispatches("raw_data/all_dispatches_final/2016_Events.xlsx")

dispatch_17 <- read_dispatches("raw_data/all_dispatches_final/2017_Events.xlsx")

dispatch_18 <- read_dispatches("raw_data/all_dispatches_final/2018_Events.xlsx")

dispatch_19 <- read_dispatches("raw_data/all_dispatches_final/2019_Events.xlsx")

dispatch_20 <- read_dispatches("raw_data/all_dispatches_final/2020_Events.xlsx")

dispatch_21 <- read_dispatches("raw_data/all_dispatches_final/2021_Events.xlsx")

dispatch_22 <- read_dispatches("raw_data/all_dispatches_final/2022_Events.xlsx")

all_dispatches <- dispatch_16 %>% 
  bind_rows(dispatch_17) %>% 
  bind_rows(dispatch_18) %>% 
  bind_rows(dispatch_19) %>% 
  bind_rows(dispatch_20) %>% 
  bind_rows(dispatch_21) %>% 
  bind_rows(dispatch_22)

all_dispatches %>% 
  write_csv("created_data/dispatches_all.csv")
