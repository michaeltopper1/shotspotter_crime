library(tidyverse)
library(readxl)

dispatch_16_1 <- read_excel("raw_data/dispatches_cpd_final_june/2016_Events.xlsx",
                            sheet = "Data")
## I'm only reading them in like this because 1) it takes forever
## and 2) map_df sometimes runs into errors if column types are not the same
## and 3) because there are multiple sheets within an excel file.
read_dispatches <- function(file) {
  sheets <- excel_sheets(file)
  final_data <- map_df(sheets, ~read_xlsx(file, sheet = . ,
                                          col_names = colnames(dispatch_16_1))) %>% 
    janitor::clean_names()
  return(final_data)
}


# reading in all data -----------------------------------------------------

dispatch_16 <- read_dispatches("raw_data/dispatches_cpd_final_june/2016_Events.xlsx")

dispatch_16 <- dispatch_16 %>% 
  janitor::row_to_names(1)

dispatch_17 <- read_dispatches("raw_data/dispatches_cpd_final_june/2017_Events.xlsx")

dispatch_17 <- dispatch_17 %>% 
  janitor::row_to_names(1)

dispatch_18 <- read_dispatches("raw_data/dispatches_cpd_final_june/2018_Events.xlsx")

dispatch_18 <- dispatch_18 %>% 
  janitor::row_to_names(1)

dispatch_19 <- read_dispatches("raw_data/dispatches_cpd_final_june/2019_Events.xlsx")

dispatch_19 <- dispatch_19 %>% 
  janitor::row_to_names(1)

dispatch_20 <- read_dispatches("raw_data/dispatches_cpd_final_june/2020_Events.xlsx")

dispatch_20 <- dispatch_20 %>% 
  janitor::row_to_names(1)

dispatch_21 <- read_dispatches("raw_data/dispatches_cpd_final_june/2021_Events.xlsx")

dispatch_21 <- dispatch_21 %>% 
  janitor::row_to_names(1)

dispatch_22 <- read_dispatches("raw_data/dispatches_cpd_final_june/2022_Events.xlsx")

dispatch_22 <- dispatch_22 %>% 
  janitor::row_to_names(1)

all_dispatches <- dispatch_16 %>% 
  bind_rows(dispatch_17, dispatch_18, dispatch_19, dispatch_20, dispatch_21, dispatch_22)

all_dispatches %>% 
  write_csv("created_data/dispatches_all.csv")
