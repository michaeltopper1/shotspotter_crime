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
    round(2) %>% 
    as_tibble() %>% rename(mean = value)
  nobs <- model$nobs %>% as_tibble() %>% rename(nobs = value) %>% 
    mutate(nobs = nobs %>% scales::comma())
  results <- estimates %>% 
    mutate(estimate = round(estimate, 2)) %>% 
    bind_cols(nobs, mean) %>% 
    mutate(term_expand = glue::glue("{term}\nMean: {mean} N: {nobs}")) %>% 
    mutate(percent_change = estimate/mean)
  return(results)
}

# priority 1 --------------------------------------------------------------


p1_0 <- dispatch_panel_p3 %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Pooled Estimate", 1)

p1_1 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "DISTURBANCE") %>% 
  feols(entry_to_dispatch ~treatment  + ..ctrl) %>% 
  tidy_estimates("Disturbance", 3)


p1_2 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "AUTO ACCIDENT PD") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 1", 3)


p1_3 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "DISTURBANCE - MUSIC/NOISE") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Disturbance: Noise", 3)

p1_4 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "PARKING VIOL. 1") %>% 
  feols(entry_to_dispatch~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 1", 3)

p1_5 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "PARKING VIOL. 2") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 2", 3)


# call to on scene --------------------------------------------------------




po1_0 <- dispatch_panel_p3 %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Pooled Estimate", 1)

po1_1 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "DISTURBANCE") %>% 
  feols(entry_to_onscene ~treatment  + ..ctrl) %>% 
  tidy_estimates("Disturbance", 3)


po1_2 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "AUTO ACCIDENT PD") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 1", 3)


po1_3 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "DISTURBANCE - MUSIC/NOISE") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Disturbance: Noise", 3)

po1_4 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "PARKING VIOL. 1") %>% 
  feols(entry_to_onscene~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 1", 3)

po1_5 <- dispatch_panel_p3 %>%
  filter(final_dispatch_description == "PARKING VIOL. 2") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Parking Violation 2", 3)


priority_3_graph <- p1_0 %>% 
  bind_rows(p1_1,
            p1_2, p1_3,
            p1_4, p1_5,
            po1_0,
            po1_1, po1_2,
            po1_3, po1_4,
            po1_5) %>% 
  mutate(outcome = case_when(
    row_number() <= 6 ~ "Call-to-Dispatch",
    row_number() >6 ~ "Call-to-On-Scene"
  )) %>% 
  mutate(aggregate = if_else(str_detect(term,"^Agg"), "1", "0")) %>% 
  mutate(across(starts_with("conf"), ~./mean )) %>% 
  mutate(main_estimate = if_else(term == "Pooled Estimate", "Color1", "Color2")) %>% 
  mutate(term_expand = fct_reorder(term_expand, mean) , .by = c(main_estimate, aggregate)) %>%
  mutate(term_expand = fct_rev(term_expand)) %>% 
  ggplot(aes(term_expand, percent_change,
             color = main_estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .1, linewidth = .7) +
  geom_hline(aes(yintercept = 0), color = "black",
             linetype = "dashed") +
  coord_flip() +
  scale_y_continuous(label = scales::label_percent()) +
  facet_wrap(~outcome,scales = "free", ncol = 2) +
  ggthemes::scale_color_wsj() +
  labs(x = "", y = "Percent Change from Mean and 95% Confidence Interval") +
  theme_minimal() +
  theme(legend.position = "non")
ggsave(priority_3_graph, filename = "presentations/figures/priority_3_graph.jpeg",
       width = 7, height = 5)
