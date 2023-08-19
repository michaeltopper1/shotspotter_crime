

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

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)

es_data_dispatch <- dispatch_panel_p1 %>% 
  mutate(time_to_treat = time_length(as_date(date) - shotspot_activate,
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


entry_1 <- es_data_dispatch %>% 
  feols(entry_to_dispatch ~ i(time_to_treat, ref = c(-1, -1000)) +
          ..ctrl,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:37) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Call-to-Dispatch")

eos_1 <- es_data_dispatch %>% 
  feols(entry_to_onscene ~ i(time_to_treat, ref = c(-1, -1000)) +
          ..ctrl,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:37) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Call-to-On-Scene")


missing_data_trend <- bind_rows(entry_1,eos_1) %>% 
  filter(periods %in% c(-11:23)) %>% 
  ggplot(aes(periods, estimate, color = type)) +
  geom_point() +
  geom_line() +
  labs(x = "Months to ShotSpotter Implementation",
       y = "Event Study Point Estimate (OLS)", 
       color = "") +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  theme(legend.position = "bottom")
