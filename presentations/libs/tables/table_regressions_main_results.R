## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-14
##

library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}

dispatch_panel <- dispatch_panel %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district)

# panel A -----------------------------------------------------------------

## Entry to dispatch
entry_1 <- feols(entry_to_dispatch_1 ~ treatment | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_2 <- feols(entry_to_dispatch_1 ~ treatment + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3+ number_dispatches_0 | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_3 <- did2s(data = dispatch_panel,
                 yname = "entry_to_dispatch_1",
                 first_stage = ~officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3+ number_dispatches_0 | district + date,
                 second_stage = ~treatment,
                 treatment = "treatment",
                 cluster_var = "district")

entry_4 <- feols(entry_to_dispatch_1 ~ treatment + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3+ number_dispatches_0 + shotspot_border_treatment| district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_5 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)


entry_6 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ treatment  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)




# entry to onscene --------------------------------------------------------


entry_os_1 <- feols(entry_to_onscene_1 ~ treatment | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_os_2 <- feols(entry_to_onscene_1 ~ treatment + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3+ number_dispatches_0 | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_os_3 <- did2s(data = dispatch_panel,
                 yname = "entry_to_onscene_1",
                 first_stage = ~officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3+ number_dispatches_0 | district + date,
                 second_stage = ~treatment,
                 treatment = "treatment",
                 cluster_var = "district")

entry_os_4 <- feols(entry_to_onscene_1 ~ treatment + officer_hours +
                      number_dispatches_1 + number_dispatches_2 +
                      number_dispatches_3+ number_dispatches_0 + shotspot_border_treatment| district + date,
                    cluster = ~district,
                    data = dispatch_panel)

entry_os_5 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ treatment  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)


entry_os_6 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ treatment  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)


# -------------------------------------------------------------------------


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01"
                      ), ~str_remove_all(., "\n"))


dispatch_table_dispatch <- panelsummary_raw(list( entry_2,
                                    entry_3, entry_5, entry_6),
                               stars = "econ",
                               mean_dependent = T,
                               coef_map = c("treatment" = "ShotSpotter Activated",
                                            "shotspot_border_treatment" = "Border District Activated"),
                               gof_omit = "^R|A|B|S",
                               gof_map = gof_mapping) 

dispatch_table_dispatch <- dispatch_table_dispatch %>% 
  janitor::clean_names() %>% 
  mutate(model_2 = if_else(term == "FE: District" |
                               term == "FE: Day-by-Month-by-Year", "X", model_2)) %>% 
  mutate(model_2 = if_else(term == "Mean of Dependent Variable", 
                             model_1, model_2)) %>% 
  add_row(term = "Gardner (2021) Robust", model_1 = "", model_2 = "X", model_3 = "", model_4 = "") %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.integer() %>%  scales::comma(), .))) %>% 
  clean_raw() %>% 
  add_header_above(c(" " = 3, "Officer Hours > Median", "<= Median")) %>% 
   footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria")

os_table <- panelsummary_raw(list(entry_os_2,
                                  entry_os_3, entry_os_5, entry_os_6),
                             stars = "econ",
                             mean_dependent = T,
                             coef_map = c("treatment" = "ShotSpotter Activated",
                                          "shotspot_border_treatment" = "Border District Activated"),
                             gof_omit = "^R|A|B|S",
                             gof_map = gof_mapping) 
os_table <- os_table %>% 
  janitor::clean_names() %>% 
  mutate(model_2 = if_else(term == "FE: District" |
                             term == "FE: Day-by-Month-by-Year", "X", model_2)) %>% 
  mutate(model_2 = if_else(term == "Mean of Dependent Variable", 
                           model_1, model_2)) %>% 
  add_row(term = "Gardner (2021) Robust", model_1 = "", model_2 = "X", model_3 = "", model_4 = "") %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.integer() %>%  scales::comma(), .))) %>% 
  clean_raw() %>% 
  add_header_above(c(" " = 3, "Officer Hours > Median", "<= Median")) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria")
