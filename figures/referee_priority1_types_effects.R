## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-17
##

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



dispatch_descriptions <- dispatch_panel_p1 %>% 
  count(final_dispatch_description, sort = T) %>% 
  head(21) %>% 
  pull(final_dispatch_description)

dispatch_descriptions <- dispatch_descriptions[dispatch_descriptions != "MENTAL HEALTH DISTURBANCE"]



total_dispatches <- dispatch_panel_p1 %>% nrow()
total_dispatches_onscene <- dispatch_panel_p1 %>% 
  drop_na(entry_to_onscene) %>% 
  nrow()


sum_stats_descriptions_dispatch <- dispatch_panel_p1 %>% 
  filter(final_dispatch_description %in% dispatch_descriptions) %>% 
  select(entry_to_dispatch,
         entry_to_onscene,
         final_dispatch_description) %>% 
  summarize(across(c(entry_to_dispatch),
                   list("mean" = ~mean(.x,na.rm = T),
                        "sd" = ~sd(.x,na.rm = T))),
            n = n(),.by = final_dispatch_description) %>% 
  mutate(total = total_dispatches,
         fraction = n/total)

sum_stats_descriptions_onscene <- dispatch_panel_p1 %>% 
  filter(final_dispatch_description %in% dispatch_descriptions) %>% 
  select(entry_to_dispatch,
         entry_to_onscene,
         final_dispatch_description) %>% 
  drop_na(entry_to_onscene) %>% 
  summarize(across(c(entry_to_onscene),
                   list("mean" = ~mean(.x,na.rm = T),
                        "sd" = ~sd(.x,na.rm = T))),
            n = n(),.by = final_dispatch_description) %>% 
  mutate(total = total_dispatches_onscene,
         fraction = n/total)

sum_stats_descriptions <- sum_stats_descriptions_dispatch %>% 
  left_join(sum_stats_descriptions_onscene, join_by(final_dispatch_description)) %>% 
  select(-starts_with("total"))


dispatch_p1_types <- map_df(dispatch_descriptions, function(description) {dispatch_panel_p1 %>% 
  filter(final_dispatch_description == description) %>% 
  feols(entry_to_dispatch ~ treatment |district + date +
          final_dispatch_description + hour) %>% 
  broom::tidy(conf.int = T)} %>% 
  mutate(dispatch_type = {{description}})) %>% 
  mutate(outcome = "Call-to-Dispatch")

onscene_p1_types <- map_df(dispatch_descriptions, function(description) {dispatch_panel_p1 %>% 
    filter(final_dispatch_description == description) %>% 
    feols(entry_to_onscene ~ treatment |district + date +
            final_dispatch_description + hour) %>% 
    broom::tidy(conf.int = T)} %>% 
      mutate(dispatch_type = {{description}})) %>% 
  mutate(outcome = "Call-to-On-Scene")


results <- dispatch_p1_types %>% 
  bind_rows(onscene_p1_types) %>% 
  left_join(sum_stats_descriptions,
            join_by(outcome == outcome,
                    dispatch_type == final_dispatch_description)) %>% 
  mutate(percent_change = estimate/mean) %>% 
  mutate(dispatch_type = str_to_title(dispatch_type)) %>% 
  mutate(dispatch_type = str_replace(dispatch_type, "Jo", "JO"),
         dispatch_type = str_replace(dispatch_type, "Ip", "IP"))


# table -------------------------------------------------------------------

footnote <- map(list( "Units are in seconds. Data is at
         the call-level. Call-to-Dispatch represents 
         the amount of time from the 911 call to an officer dispatching
         to the scene. Call-to-On-Scene is the time from a 911 call to
         when an officer arrives on-scene.
         Priority 1 Call-to-On-Scene is missing approximately 45 percent
         of on-scene times, although the results
         remain consistent when we can observe both response times. The Fraction
         Column
         represents the fraction of the total amount of each outcome. Any description
         ending with IP stands for `in progress.'
                  "), ~str_remove_all(., "\n"))

sum_stats_descriptions_table <- sum_stats_descriptions %>% 
  mutate(final_dispatch_description = str_to_title(final_dispatch_description)) %>% 
  mutate(final_dispatch_description = str_replace(final_dispatch_description, "Jo", "JO"),
         final_dispatch_description = str_replace(final_dispatch_description, "Ip", "IP")) %>%
  mutate(across(matches("mean|sd|fraction"), ~sprintf("%.2f", .))) %>% 
  mutate(n.x = scales::comma(n.x),
         n.y = scales::comma(n.y)) %>% 
  kbl(col.names = c("Dispatch Description", "Mean", "SD", "N", "Fraction","Mean", "SD", "N", "Fraction"),
      booktabs = T,
      caption = "\\label{}") %>% 
  kable_styling() %>% 
  add_header_above(c(' ' = 1,
                     'Call-to-Dispatch' = 4,
                     'Call-to-On-Scene' = 4)) %>% 
  footnote(footnote, threeparttable = T)


# figure ------------------------------------------------------------------


priority_1_descriptions <- results %>% 
  ggplot(aes(estimate, dispatch_type)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.2) +
  geom_vline(xintercept = 0, color = "dark red", linetype = "dashed") +
  facet_wrap(~outcome) +
  scale_y_discrete(limits = rev) +
  labs(x = "Point Estimate and 95% Confidence Interval",
       y = "") +
  theme_minimal()
