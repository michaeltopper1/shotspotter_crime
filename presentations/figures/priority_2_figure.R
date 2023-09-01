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


p1_0 <- dispatch_panel_p2 %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Aggregate Estimate", 1)

p1_1 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "ALARM BURGLAR") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Alarm Burglar", 2)


p1_2 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "ALARM COMMERCIAL") %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates("Alarm Commercial", 2)


p1_3 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "SUSPICIOUS AUTO WITH OCC") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Suspicious Auto", 2)

p1_4 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "PERSON DOWN") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Person Down", 2)

p1_5 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "AUTO ACCIDENT PI") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Auto Accident PI", 2)


# call to on scene --------------------------------------------------------




po1_0 <- dispatch_panel_p2 %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Aggregate Estimate", 1)

po1_1 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "ALARM BURGLAR") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Alarm Burglar", 2)


po1_2 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "ALARM COMMERCIAL") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Alarm Commercial", 2)


po1_3 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "SUSPICIOUS AUTO WITH OCC") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Suspicious Auto", 2)

po1_4 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "PERSON DOWN") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Person Down", 2)



po1_5 <- dispatch_panel_p2 %>%
  filter(final_dispatch_description == "AUTO ACCIDENT PI") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Auto Accident PI", 2)


priority_2_graph <- p1_0 %>% 
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
  mutate(across(starts_with("conf"), ~./mean )) %>% 
  mutate(term_expand = fct_reorder(term_expand, -row_number()), .by = outcome) %>% 
  ggplot(aes(term_expand, percent_change, shape = outcome,
             color = outcome)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .1, linewidth = 0.8) +
  geom_hline(aes(yintercept = 0), color = "black",
             linetype = "dashed") +
  coord_flip() +
  scale_y_continuous(label = scales::label_percent()) +
  facet_wrap(~outcome,scales = "free", ncol = 2) +
  ggthemes::scale_color_wsj() +
  labs(x = "", y = "Percent Change from Mean and 95% Confidence Interval") +
  theme_minimal() +
  theme(legend.position = "non")

ggsave(priority_2_graph, filename = "presentations/figures/priority_2_graph.jpeg",
       width = 7, height = 5)

