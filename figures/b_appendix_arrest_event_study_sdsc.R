
library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(modelsummary)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}


setFixest_fml(..ctrl = ~0| district + date + hour + final_dispatch_description +
                treatment_sdsc)

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

event_study_graph <- function(x){
  graph <- x %>% 
    ggplot(aes(periods, estimate, color = type, shape = type)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, color = "dark red") +
    labs(x = "Months to Adoption",
         y = "Point Estimate (pp.) and 95% Confidence Interval",
         color = "",
         shape = "") +
    theme_minimal() +
    ggthemes::scale_color_stata() +
    theme(legend.position = "bottom")
  return(graph)
}



arrest_1 <- es_data_dispatch %>% 
  feols(arrest_made ~ i(time_to_treat, ref = c(-1, -1000)) +
          ..ctrl,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:37) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "OLS")


arrest_1_2sdid <- did2s(es_data_dispatch,
                        yname = "arrest_made",
                        first_stage = ~..ctrl,
                        second_stage = ~ i(time_to_treat, ref = c(-1, -1000)),
                        treatment = "treatment",
                        cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Gardner (2021)")

arrest_1_es_sdsc <- arrest_1 %>% 
  bind_rows(arrest_1_2sdid) %>% 
  filter(periods %in% c(-11:23)) %>% 
  event_study_graph()




# saving ------------------------------------------------------------------

ggsave(arrest_1_es_sdsc, filename = "paper/appendix_figures/appendix_event_study_arrests_sdsc.jpeg", width = 7, height = 5)
