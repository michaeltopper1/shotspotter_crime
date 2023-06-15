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
  dispatch_panel <- read_csv("analysis_data/xxdispatch_panel.csv")
}

dispatch_panel <- dispatch_panel %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T))

# panel A -----------------------------------------------------------------

## Entry to dispatch
entry_1 <- feols(entry_to_dispatch_1 ~ treatment | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_2 <- feols(entry_to_dispatch_1 ~ treatment + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_3 <- did2s(data = dispatch_panel,
                 yname = "entry_to_dispatch_1",
                 first_stage = ~officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 | district + date,
                 second_stage = ~treatment,
                 treatment = "treatment",
                 cluster_var = "district")

entry_4 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)


entry_5 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ treatment  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)




# entry to onscene --------------------------------------------------------


entry_os_1 <- feols(entry_to_onscene_1 ~ treatment | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_os_2 <- feols(entry_to_onscene_1 ~ treatment + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_os_3 <- did2s(data = dispatch_panel,
                 yname = "entry_to_onscene_1",
                 first_stage = ~officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 | district + date,
                 second_stage = ~treatment,
                 treatment = "treatment",
                 cluster_var = "district")

entry_os_4 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ treatment  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)


entry_os_5 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ treatment  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)


# -------------------------------------------------------------------------


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Panel A shows the time from entry call to dispatched officer.
                  Panel B shows time from the dispatched officer to on scene. Controls
                  in all models include controls for officer hours and number of dispatches.
                  "), ~str_remove_all(., "\n"))


dispatch_table <- panelsummary_raw(list(entry_1, entry_2,
                                    entry_3, entry_4, entry_5),
                               list(entry_os_1, entry_os_2,
                                    entry_os_3, entry_os_4, entry_os_5),
                               stars = "econ",
                               mean_dependent = T,
                               coef_map = c("treatment" = "ShotSpotter Activated"),
                               gof_omit = "^R|A|B|S",
                               caption = "\\label{dispatch_table}Effect of ShotSpotter Rollout on Response Times (OLS)",
                               gof_map = gof_mapping) 

dispatch_table %>% 
  janitor::clean_names() %>% 
  slice(-c(5:6)) %>% 
  mutate(model_3 = if_else(term == "FE: District" |
                               term == "FE: Day-by-Month-by-Year", "X", model_3)) %>% 
  mutate(model_3 = if_else(term == "Mean of Dependent Variable", 
                             model_2, model_3)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "", model_2 = "",
          model_3 = "", model_4 = "", model_5 = "", .before = 5) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "", model_2 = "",
          model_3 = "", model_4 = "", model_5 = "", .before = 10) %>% 
  add_row(term = "Control Variables", model_1 = "", model_2 = "X", model_3 = "X", model_4 = "X",
          model_5 = "X") %>% 
  add_row(term = "Gardner (2021) Robust", model_1 = "", model_2 = "", model_3 = "X", model_4 = "",
          model_5 = "") %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.double() %>% as.integer() %>% scales::comma(), .))) %>% 
  clean_raw() %>% 
  pack_rows("Panel A: Call to Dispatch",1,5, italic = T, bold = F, hline_after = F) %>% 
  pack_rows("Panel B: Call to On-Scene", 6, 10, italic = T, bold = F) %>% 
  row_spec(10, hline_after = TRUE)


  add_header_above(c(" " = 4,
                     "Priority 2" = 1,
                     "Priority 3" = 1)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_paper()


