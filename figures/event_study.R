## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-01
##

library(tidyverse)
library(fixest)
library(modelsummary)

stops_panel <- read_csv("analysis_data/stops_panel.csv")

leead <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lead(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}
seq(from = -3000, to = 3000, by = 30)
labels <- c(-3000:3000)

stops_panel_es %>% 
  count(lead_1, district) %>% View()
stops_panel_es <- stops_panel %>% 
  arrange(date) %>% 
  mutate(first_day_treatment = ifelse(date == shotspot_activate, 1, 0)) %>%
  mutate(lead_1 = leead(first_day_treatment, c(1:30)),
         lead_2 = leead(first_day_treatment, c(31:60)),
         lead_3 = leead(first_day_treatment, c(61:90)),
         lead_4 = leead(first_day_treatment, c(91:120)),
         lead_5 = leead(first_day_treatment, c(121:150)),
         lead_6 = leead(first_day_treatment, c(151:180)),
         lead_7 = leead(first_day_treatment, c(181:210)),
         lead_8 = leead(first_day_treatment, c(211:240)),
         lead_9 = leead(first_day_treatment, c(241:270)), .by = district) %>% 
  group_by(district) %>% 
  arrange(date) %>% 
  mutate(lead_9 = ifelse(lead_9 == 0, NA, lead_9)) %>% 
  fill(lead_9, .direction = "up") %>% 
  mutate(lead_9 = ifelse(is.na(lead_9), 0, lead_9)) %>% 
  ungroup()
stops_panel_es <- stops_panel_es %>% 
  arrange(date) %>% 
  mutate(lag_0 = laag(first_day_treatment, c(0:29)),
         lag_1 = laag(first_day_treatment, c(30:59)),
         lag_2 = laag(first_day_treatment, c(60:89)),
         lag_3 = laag(first_day_treatment, c(90:119)),
         lag_4 = laag(first_day_treatment, c(120:149)),
         lag_5 = laag(first_day_treatment, c(150:179)),
         lag_6 = laag(first_day_treatment, c(180:209)),
         lag_7 = laag(first_day_treatment, c(210:239)),
         lag_8 = laag(first_day_treatment, c(240:269)),
         lag_9 = laag(first_day_treatment, c(270:299)),
         .by = district) %>% 
  group_by(district) %>% 
  mutate(lag_9 = ifelse(lag_9 == 0, NA, lag_9)) %>% 
  fill(lag_9, .direction = "down") %>% 
  mutate(lag_9 = ifelse(is.na(lag_9), 0, lag_9)) %>% 
  ungroup(district) 

iplot_test <- stops_panel %>% 
  mutate(months_to_treat = ifelse(date < shotspot_activate, (interval(shotspot_activate, date) %/% months(1)) - 1,
                                  interval(shotspot_activate, date) %/% months(1)),.before = 1) %>% 
  mutate(months_to_treat = ifelse(never_treated == 1, -1000, months_to_treat)) %>% 
  mutate(months_to_treat = ifelse(months_to_treat > 5, 5, months_to_treat)) %>% 
  mutate(months_to_treat = ifelse(months_to_treat < -5, -5, months_to_treat)) 

iplot_test %>% 
  mutate(cohort = ifelse(months_to_treat == 0, 0, NA)) %>% 
  feols(number_black_stops ~ sunab(cohort, months_to_treat) |district + date,
        cluster = ~district, data = .) %>% iplot()

base_stagg %>% View()
iplot_test %>% 
  mutate(treated = ifelse(never_treated == 0, 1, 0)) %>% 
  mutate(year_treated = ifelse(never_treated == 1, -1000))
  feols(number_stops ~ sunab(months_to_treat, treated, ref = c(-1, -1000)) |district + date,
                               cluster = ~district, data = .) %>% iplot()
  summary()

stops_panel %>% 
  filter(year < 2020) %>% 
  feols(number_black_stops ~treatment | date + district ,
        cluster = ~district, 
        data = .)
stops_panel_es %>% 
  mutate(across(starts_with("lead"), ~ifelse(district == 16  | district == 17, 0, .))) %>% 
  mutate(across(starts_with("lag"), ~ifelse(district == 16 | district == 17, 0, .))) %>% 
  # filter(never_treated == 0) %>% 
  # filter(district != 16 & district != 17) %>%
  feols(number_black_stops ~  lead_9 + lead_8 + lead_7 +
          lead_6 + lead_5 + lead_4 + lead_3 +lead_2 +
          lag_0 + lag_1 + lag_2 + lag_3 + lag_4 +
          lag_5 + lag_6 + lag_7 + lag_8 + lag_9 |
          district + date,
        cluster = ~district,
        data = .) %>% 
  summary()


stops
stops_panel %>% 
  arrange(desc(date)) %>% 
  mutate(lead_1 = date - shotspot_activate, .by = district) %>% 
  mutate(lead_1 = as.double(lead_1)) %>% 
  mutate(lead_1_group = cut(lead_1, breaks = seq(from = -3000, to = 3000, by = 30), right = T),
         .before = 1) %>% 
  filter(district == 7) %>% relocate(date, lead_1, treatment) %>% View()
