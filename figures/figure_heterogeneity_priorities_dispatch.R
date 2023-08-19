library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}


if (!exists("dispatch_panel_p2")) {
  dispatch_panel_p2 <- dispatch_panel %>% 
    filter(priority_code ==2) 
  dispatch_panel_p3 <- dispatch_panel %>% 
    filter(priority_code ==3) 
}
setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)


# cleaning functino -------------------------------------------------------

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


p1_0 <- dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Aggregate Estimate", 1)
  
p1_1 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "DOMESTIC DISTURBANCE") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Domestic Disturbance", 1)
  

p1_2 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "CHECK WELL BEING") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Check Well Being", 1)

p1_3 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "BATTERY IP") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Battery In Progress", 1)

p1_4 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "SUSPICIOUS PERSON") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Suspicious Person", 1)

p1_5 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "EMS") %>%  
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("EMS", 1)


# priority 2 --------------------------------------------------------------

p2_0 <- dispatch_panel_p2 %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Aggregate Estimate", 2)

p2_1 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "ALARM BURGLAR") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Alarm Burglar", 2)

p2_2 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "ALARM COMMERCIAL") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Alarm Commercial", 2)


p2_3 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "SUSPICIOUS AUTO WITH OCC") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Suspicious Auto", 2)

p2_4 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "PERSON DOWN") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Person Down", 2)

p2_5 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "AUTO ACCIDENT PI") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Auto Accident PI", 2)



# priority 3 --------------------------------------------------------------

p3_0 <- dispatch_panel_p3 %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Aggregate Estimate", 3)

p3_1 <- dispatch_panel_p3 %>% 
  filter(final_dispatch_description == "DISTURBANCE") %>% 
  feols(entry_to_dispatch ~treatment  + ..ctrl) %>% 
  tidy_estimates("Disturbance", 3)

p3_2 <- dispatch_panel_p3 %>% 
  filter(final_dispatch_description == "AUTO ACCIDENT PD") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 1", 3)

p3_3 <- dispatch_panel_p3 %>% 
  filter(final_dispatch_description == "DISTURBANCE - MUSIC/NOISE") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Disturbance: Noise", 3)

p3_4 <- dispatch_panel_p3 %>% 
  filter(final_dispatch_description == "PARKING VIOL. 1") %>% 
  feols(entry_to_dispatch~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 1", 3)

p3_5 <- dispatch_panel_p3 %>% 
  filter(final_dispatch_description == "PARKING VIOL. 2") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 2", 3)


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

priorities_dispatch <- descriptions %>% 
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
