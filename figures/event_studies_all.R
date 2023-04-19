## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-14
##

library(tidyverse)
library(fixest)
library(modelsummary)

crimes <- read_csv("analysis_data/crimes_panel.csv")

stops <- read_csv("analysis_data/stops_panel.csv")


## use the months(1) to change how many months you want in a bin
## use the months to treat to give the amount of leads/lags
es_data <- crimes %>% 
  mutate(months_to_treat = ifelse(date < shotspot_activate, (interval(shotspot_activate, date) %/% months(1)) - 1,
                                  interval(shotspot_activate, date) %/% months(1)),.before = 1) %>% 
  mutate(months_to_treat = ifelse(never_treated == 1, -1000, months_to_treat)) %>% 
  mutate(months_to_treat = ifelse(months_to_treat > 12, 12, months_to_treat)) %>% 
  mutate(months_to_treat = ifelse(months_to_treat < -12, -12, months_to_treat)) 

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



# graphs ------------------------------------------------------------------

gun_arrests_es <- es_data %>% 
  feols(number_gun_involved_arrest ~ i(months_to_treat, ref = c(-1, -1000)) |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()


other_crime_es <- es_data %>% 
  feols(number_other_crime ~ i(months_to_treat, ref = c(-1, -1000)) |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()

other_arrest_es <- es_data %>% 
  feols(number_other_arrest ~ i(months_to_treat, ref = c(-1, -1000)) |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()

gun_involved_es <- es_data %>% 
  feols(number_gun_involved ~ i(months_to_treat, ref = c(-1, -1000)) |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()
  

number_black_stops_es <- es_data_stops %>% 
  feols(number_black_stops ~ i(months_to_treat, ref = c(-1, -1000)) |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()


number_firearm_found_es <- es_data_stops %>% 
  feols(number_firearm_found ~ i(months_to_treat, ref = c(-1, -1000)) |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:12)) %>% 
  event_study_graph()

