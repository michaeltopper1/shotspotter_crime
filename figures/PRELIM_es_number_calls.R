

library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}


## controls
setFixest_fml(..ctrl = ~0| district + date )


agg_outcomes <- read_csv('analysis_data/xxaggregate_outcomes.csv')


aggregate_calls <- dispatch_panel %>% 
  group_by(date, district) %>% 
  summarize(number_dispatches = n()) %>% ungroup()

aggregate_p1_calls <- dispatch_panel_p1 %>% 
  group_by(date, district) %>% 
  summarize(number_dispatches_p1 = n()) %>% ungroup()

aggregate_calls_no_gun <- dispatch_panel %>% 
  filter(gun_crime_report != 1) %>% 
  group_by(date, district) %>% 
  summarize(number_dispatches_no_gun = n()) %>% ungroup()

aggregate_p1_calls_no_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report != 1) %>% 
  group_by(date, district) %>% 
  summarize(number_dispatches_p1_no_gun = n()) %>% ungroup()

agg_outcomes <- agg_outcomes %>% 
  left_join(aggregate_calls) %>% 
  left_join(aggregate_p1_calls) %>% 
  left_join(aggregate_calls_no_gun) %>% 
  left_join(aggregate_p1_calls_no_gun)


## getting rid of those days we restrict the sample to. 
agg_outcomes <- agg_outcomes %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date))

agg_outcomes %>% 
  filter(is.na(number_dispatches)) %>% 
  distinct(date)

agg_outcomes <- agg_outcomes %>% 
  filter(!(day == 4 & month == 7 ) &
           !(day == 1 & month == 1) &
           !(day == 31 & month == 12))

# setting up event study --------------------------------------------------


agg_outcomes <- agg_outcomes %>% 
  mutate(time_to_treat = time_length(date - shotspot_activate,
                                     "month") %>% 
           magrittr::add(1) %>% 
           floor() %>%
           magrittr::subtract(1),
         .by = district) %>% 
  mutate(time_to_treat = case_when(
    time_to_treat > 24 ~ 24,
    time_to_treat < -12 ~ -12,
    .default = time_to_treat
  )) %>% 
  mutate(time_to_treat = if_else(is.na(time_to_treat), -1000, time_to_treat)) 





# calls -------------------------------------------------------------------

agg_outcomes <- agg_outcomes %>% 
  mutate(month = month(date),.before = 1) %>% 
  mutate(summer = if_else(month %in% c(6:9), 1, 0))

dispatch_panel %>% 
  distinct(final_dispatch_code)
agg_outcomes %>% 
  feols(number_dispatches ~ treatment | district + date)
agg_outcomes <- agg_outcomes %>% 
  mutate(year = year(date),
         day = day(date))

agg_outcomes %>% 
  ggplot(aes(log(number_dispatches))) +
  geom_histogram()
agg_outcomes <- agg_outcomes %>% 
  mutate(log_number_dispatches = log(number_dispatches))
did2s(agg_outcomes,
      yname = "log_number_dispatches", first_stage = ~ 0 | district + date,
      second_stage = ~ i(time_to_treat, ref = c(-1, -1000)), treatment = "treatment",
      cluster_var = "district"
) %>% iplot()
agg_outcomes %>% 
  fepois(number_dispatches_p1~ i(time_to_treat, ref = c(-1, -1000))| district + date,
         cluster = ~district, data = .) %>% 
  iplot()

agg_outcomes %>% 
  filter(is.na(number_dispatches)) %>% View()
  fepois(number_dispatches_p1_no_gun ~ treatment |district + date)
dispatch_panel_p1 %>% 
  mutate(onscene_to_close = entry_to_close - entry_to_onscene) %>% 
  feols(onscene_to_close~ treatment | district + date + hour + final_dispatch_description)


dispatch_panel_p1 %>% 
  count(final_dispatch_description, sort = T) %>% View()
x <- dispatch_panel_p1 %>% 
  mutate(treatment = if_else(treatment == 0, "pre", 'post')) %>% 
  count(treatment, final_dispatch_code) %>% 
  filter(n > 100) %>% 
  pivot_wider(names_from = treatment, values_from = n) %>% 
  drop_na(pre, post) 
y <- x %>% slice(1:100)
y <- dispatch_panel %>% 
  filter(treatment == 0) %>% 
  count(final_dispatch_description)

ks.test(x,y)

x <- x %>% 
  mutate(post_total = sum(post, na.rm = T),
         pre_total = sum(pre, na.rm = T)) %>% 
  mutate(pre_fraction = pre/pre_total,
         post_fraction = post/post_total)

