library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

dispatch_panel <- read_csv("analysis_data/xxdispatch_panel.csv")

tidy_estimates <- function(model, description, priority) {
  estimates <- model %>% 
    broom::tidy(conf.int = T) %>% 
    filter(term == "treatment") %>% 
    mutate(term = description,
           priority = priority)
  nobs <- model$nobs %>% as_tibble() %>% rename(nobs = value) %>% mutate(nobs = nobs %>% scales::comma())
  results <- estimates %>% 
    mutate(estimate = round(estimate, 2)) %>% 
    bind_cols(nobs) %>% 
    mutate(term_expand = glue::glue("{term}\nEstimate: {estimate}\nN: {nobs}"))
  return(results)
}



# priority 1 --------------------------------------------------------------


p1_1 <- dispatch_panel %>% 
  feols(entry_to_onscene_domestic_disturb_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates(description = "Domestic Disturbance", 1)


p1_2 <- dispatch_panel %>% 
  feols(entry_to_onscene_check_well_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Check Well Being", 1)

p1_3 <- dispatch_panel %>% 
  feols(entry_to_onscene_battery_ip_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Battery In Progress", 1)

p1_4 <- dispatch_panel %>% 
  feols(entry_to_onscene_suspicious_person_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Suspicious Person", 1)

p1_5 <- dispatch_panel %>% 
  feols(entry_to_onscene_ems_p1 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("EMS", 1)


# priority 2 --------------------------------------------------------------


p2_1 <- dispatch_panel %>% 
  feols(entry_to_onscene_alarm_burglar_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Alarm Burglar", 2)

p2_2 <- dispatch_panel %>% 
  feols(entry_to_onscene_alarm_commercial_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Alarm Commercial", 2)


p2_3 <- dispatch_panel %>% 
  feols(entry_to_onscene_suspicious_auto_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Suspicious Auto", 2)

p2_4 <- dispatch_panel %>% 
  feols(entry_to_onscene_person_down_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Person Down", 2)

p2_5 <- dispatch_panel %>% 
  feols(entry_to_onscene_battery_jo_p2 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Battery Just Occurred", 2)



# priority 3 --------------------------------------------------------------

p3_1 <- dispatch_panel %>% 
  feols(entry_to_onscene_disturbance_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Disturbance", 3)

p3_2 <- dispatch_panel %>% 
  feols(entry_to_onscene_parking_violation1_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Parking Violation 1", 3)

p3_3 <- dispatch_panel %>% 
  feols(entry_to_onscene_disturbance_music_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Disturbance: Noise", 3)

p3_4 <- dispatch_panel %>% 
  feols(entry_to_onscene_parking_violation2_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Parking Violation 2", 3)

p3_5 <- dispatch_panel %>% 
  feols(entry_to_onscene_selling_narcotics_p3 ~treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  tidy_estimates("Selling Narcotics", 3)


descriptions <- p1_1 %>% 
  bind_rows(p1_2, p1_3,
            p1_4, p1_5,
            p2_1, p2_2,
            p2_3, p2_4,
            p2_5, p3_1,
            p3_2, p3_3,
            p3_4, p3_5)

descriptions %>% 
  mutate(priority = glue::glue("Priority {priority}")) %>% 
  ggplot(aes(term_expand, estimate, shape = priority,
             color = priority)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .1, size = .7) +
  geom_hline(aes(yintercept = 0), color = "black",
             linetype = "dashed") +
  coord_flip() +
  facet_wrap(~priority,scales = "free_y") +
  ggthemes::scale_color_stata() +
  labs(x = "", y = "Point Estimate and 95% Confidence Interval (Seconds)") +
  theme_minimal() +
  theme(legend.position = "none")

