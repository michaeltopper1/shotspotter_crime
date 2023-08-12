library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}

tidy_estimates <- function(model, description, priority) {
  estimates <- model %>% 
    broom::tidy(conf.int = T) %>% 
    filter(term == "treatment") %>% 
    mutate(term = description,
           priority = priority)
  mean <- model %>% fitstat(., type = "my") %>% as.numeric() %>% 
    as_tibble() %>% rename(mean = value)
  nobs <- model$nobs %>% as_tibble() %>% rename(nobs = value) %>% 
    mutate(nobs = nobs %>% scales::comma())
  results <- estimates %>% 
    mutate(estimate = round(estimate, 2)) %>% 
    bind_cols(nobs, mean) %>% 
    mutate(term_expand = glue::glue("{term}\nEstimate: {estimate} N: {nobs}")) %>% 
    mutate(percent_change = estimate/mean)
  return(results)
}



# priority 1 --------------------------------------------------------------

p1_0 <- dispatch_panel %>% 
  feols(entry_to_onscene_1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates(description = "Aggregate Estimate", 1)

p1_1 <- dispatch_panel %>% 
  feols(entry_to_onscene_domestic_disturb_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates(description = "Domestic Disturbance", 1)


p1_2 <- dispatch_panel %>% 
  feols(entry_to_onscene_check_well_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Check Well Being", 1)

p1_3 <- dispatch_panel %>% 
  feols(entry_to_onscene_battery_ip_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3| district + date) %>% 
  tidy_estimates("Battery In Progress", 1)

p1_4 <- dispatch_panel %>% 
  feols(entry_to_onscene_suspicious_person_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Suspicious Person", 1)

p1_5 <- dispatch_panel %>% 
  feols(entry_to_onscene_ems_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("EMS", 1)


# priority 2 --------------------------------------------------------------

p2_0 <- dispatch_panel %>% 
  feols(entry_to_onscene_2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Aggregate Estimate", 2)

p2_1 <- dispatch_panel %>% 
  feols(entry_to_onscene_alarm_burglar_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Alarm Burglar", 2)

p2_2 <- dispatch_panel %>% 
  feols(entry_to_onscene_alarm_commercial_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3| district + date) %>% 
  tidy_estimates("Alarm Commercial", 2)


p2_3 <- dispatch_panel %>% 
  feols(entry_to_onscene_suspicious_auto_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Suspicious Auto", 2)

p2_4 <- dispatch_panel %>% 
  feols(entry_to_onscene_person_down_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3| district + date) %>% 
  tidy_estimates("Person Down", 2)

p2_5 <- dispatch_panel %>% 
  feols(entry_to_onscene_auto_accident_pi_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3| district + date) %>% 
  tidy_estimates("Battery Just Occurred", 2)



# priority 3 --------------------------------------------------------------


p3_0 <- dispatch_panel %>% 
  feols(entry_to_onscene_3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Aggregate Estimate", 3)

p3_1 <- dispatch_panel %>% 
  feols(entry_to_onscene_disturbance_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Disturbance", 3)

p3_2 <- dispatch_panel %>% 
  feols(entry_to_onscene_parking_violation1_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Parking Violation 1", 3)

p3_3 <- dispatch_panel %>% 
  feols(entry_to_onscene_disturbance_music_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3| district + date) %>% 
  tidy_estimates("Disturbance: Noise", 3)

p3_4 <- dispatch_panel %>% 
  feols(entry_to_onscene_parking_violation2_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 | district + date) %>% 
  tidy_estimates("Parking Violation 2", 3)

p3_5 <- dispatch_panel %>% 
  feols(entry_to_onscene_auto_accident_pd_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3| district + date) %>% 
  tidy_estimates("Selling Narcotics", 3)



descriptions <- p1_0 %>% 
  bind_rows(p1_1,
            p1_2, p1_3,
            p1_4, p1_5,
            p2_0,
            p2_1, p2_2,
            p2_3, p2_4,
            p2_5,
            p3_0, p3_1,
            p3_2, p3_3,
            p3_4, p3_5)

priorities_onscene <- descriptions %>% 
  mutate(priority_description = case_when(
    priority ==1  ~ "Immediate Dispatch",
    priority == 2 ~ "Rapid Dispatch",
    priority == 3 ~ "Administrative Dispatch"
  )) %>% 
  mutate(across(starts_with("conf"), ~./mean )) %>% 
  mutate(priority = glue::glue("Priority {priority}\n({priority_description})")) %>% 
  mutate(main_estimate = if_else(term == "Aggregate Estimate", "Color1", "Color2")) %>% 
  mutate(term_expand = fct_reorder(term_expand, -row_number())) %>% 
  ggplot(aes(term_expand, percent_change, shape = priority,
             color = main_estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .1, linewidth = .7) +
  geom_hline(aes(yintercept = 0), color = "black",
             linetype = "dashed") +
  scale_y_continuous(labels =scales::percent_format()) +
  coord_flip() +
  facet_wrap(~priority,scales = "free_y", ncol = 1) +
  ggthemes::scale_color_stata() +
  labs(x = "", y = "Percent Change from Mean and 95% Confidence Interval") +
  theme_minimal() +
  theme(legend.position = "non")
