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


p1_0 <- dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Pooled Estimate", 1)

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




po1_0 <- dispatch_panel_p1 %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Pooled Estimate", 1)

po1_1 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "DOMESTIC DISTURBANCE") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates(description = "Domestic Disturbance", 1)


po1_2 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "CHECK WELL BEING") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Check Well Being", 1)

po1_3 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "BATTERY IP") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Battery In Progress", 1)

po1_4 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "SUSPICIOUS PERSON") %>% 
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("Suspicious Person", 1)



po1_5 <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "EMS") %>%  
  feols(entry_to_onscene ~treatment + ..ctrl) %>% 
  tidy_estimates("EMS", 1)


priority_1_graph <- p1_0 %>% 
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

ggsave(priority_1_graph, filename = "presentations/figures/priority_1_graph.jpeg",
       width = 7, height = 5)

