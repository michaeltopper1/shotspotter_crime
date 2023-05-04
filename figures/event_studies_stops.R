## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-14
##

library(tidyverse)
library(fixest)
library(modelsummary)

stops <- read_csv("analysis_data/stops_panel.csv")

shotspotter_borders <- read_csv("created_data/border_districts_final.csv")


# attaching borders -------------------------------------------------------

stops <- stops %>% 
  left_join(shotspotter_borders,
            join_by(district == border_district)) %>% 
  relocate(border_treatment) %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1 ,0 )) %>% 
  ungroup() 


## use the months(1) to change how many months you want in a bin
## use the months to treat to give the amount of leads/lags

es_data_stops <- stops %>% 
  mutate(months_to_treat = ifelse(date < shotspot_activate, (interval(shotspot_activate, date) %/% months(1)) - 1,
                                  interval(shotspot_activate, date) %/% months(1)),.before = 1) %>% 
  mutate(months_to_treat = ifelse(never_treated == 1, -1000, months_to_treat)) %>% 
  mutate(months_to_treat = ifelse(months_to_treat > 12, 12, months_to_treat)) %>% 
  mutate(months_to_treat = ifelse(months_to_treat < -12, -12, months_to_treat)) 

event_study_graph <- function(x){
  graph <- x %>% 
    ggplot(aes(periods, estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high )) +
    geom_hline(yintercept = 0, color = "dark red") +
    labs(x = "30-day Periods",
         y = "95% Confidence Interval and Point Estimate") +
    theme_minimal()
  return(graph)
}

search_rate_es <- es_data_stops %>% 
  filter(year < 2020) %>% 
  feols(search_rate ~i(months_to_treat, ref = c(-1,-1000)) | district + date,
        cluster = ~district) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()

number_search_es <- es_data_stops %>% 
  filter(year < 2020) %>% 
  feols(number_search ~i(months_to_treat, ref = c(-1,-1000)) | district + date,
        cluster = ~district) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()


number_stops_es <- es_data_stops %>% 
  filter(year < 2020) %>% 
  feols(number_stops ~i(months_to_treat, ref = c(-1,-1000)) | district + date,
        cluster = ~district) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()

firearm_found_rate_es <- es_data_stops %>% 
  filter(year < 2020) %>% 
  feols(firearm_found_rate ~i(months_to_treat, ref = c(-1,-1000)) | district + date,
        cluster = ~district) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()

