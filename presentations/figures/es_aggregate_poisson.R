
library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(modelsummary)
library(did2s)

agg_outcomes <- read_csv('analysis_data/xxaggregate_outcomes.csv')


## currently using SDSC control. Can change if needed. 
setFixest_fml(..ctrl = ~0| district + date )


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




# poisson for 3 outcomes --------------------------------------------------


gun_arrests_poisson <- agg_outcomes %>% 
  fepois(number_gun_arrests ~ i(time_to_treat, ref = c(-1, -1000)) + ..ctrl ,
         cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Poisson",
         outcome = "Panel A: Gun Arrests")

gun_victims_poisson <- agg_outcomes %>% 
  fepois(number_gun_involved_victims ~ i(time_to_treat, ref = c(-1, -1000)) + ..ctrl ,
         cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Poisson",
         outcome = "Panel B: Gun Victims")

gun_crimes_cleared_poisson <- agg_outcomes %>% 
  fepois(number_gun_crimes_cleared ~ i(time_to_treat, ref = c(-1, -1000)) + ..ctrl ,
         cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Poisson",
         outcome = "Panel C: Gun Crimes Cleared")

es_aggregate_poisson <- bind_rows(gun_arrests_poisson, gun_victims_poisson, gun_crimes_cleared_poisson) %>%
  filter(periods %in% c(-11:23)) %>% 
  ggplot(aes(periods, estimate, shape = outcome, color = outcome)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5),
                width = 0.5) +
  geom_hline(yintercept = 0, color = "dark red") +
  labs(x = "Months to Adoption",
       y = "Point Estimate and 95% Confidence Interval",
       color = "",
       shape = "") +
  theme_minimal() +
  geom_vline(aes(xintercept = -1), linetype = 'dashed') +
  facet_wrap(~outcome, ncol = 1) +
  ggthemes::scale_color_stata() +
  theme(legend.position = "none")

ggsave(es_aggregate_poisson, filename = "presentations/figures/es_aggregate_poisson.jpeg", width = 7, height = 5)

