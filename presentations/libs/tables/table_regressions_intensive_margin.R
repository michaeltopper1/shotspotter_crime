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
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}
dispatch_panel <- dispatch_panel %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T))

# Panel A -----------------------------------------------------------------


sst_1 <- feols(entry_to_dispatch_1 ~ number_sst_alerts | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

sst_2 <- feols(entry_to_dispatch_1 ~ number_sst_alerts + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

sst_3 <- did2s(data = dispatch_panel,
                 yname = "entry_to_dispatch_1",
                 first_stage = ~officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 | district + date,
                 second_stage = ~number_sst_alerts,
                 treatment = "treatment",
                 cluster_var = "district")

sst_4 <- feols(entry_to_dispatch_1 ~ number_sst_alerts + officer_hours +
                 number_dispatches_1 + number_dispatches_2 +
                 number_dispatches_3 + shotspot_border_treatment| district + date,
               cluster = ~district,
               data = dispatch_panel)


sst_5 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ number_sst_alerts + officer_hours + 
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)


sst_6 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ number_sst_alerts  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)




# entry to onscene --------------------------------------------------------


sst_os_1 <- feols(entry_to_onscene_1 ~ number_sst_alerts | district + date,
                    cluster = ~district,
                    data = dispatch_panel)

sst_os_2 <- feols(entry_to_onscene_1 ~ number_sst_alerts + officer_hours +
                      number_dispatches_1 + number_dispatches_2 +
                      number_dispatches_3 | district + date,
                    cluster = ~district,
                    data = dispatch_panel)

sst_os_3 <- did2s(data = dispatch_panel,
                    yname = "entry_to_onscene_1",
                    first_stage = ~officer_hours +
                      number_dispatches_1 + number_dispatches_2 +
                      number_dispatches_3 | district + date,
                    second_stage = ~number_sst_alerts,
                    treatment = "treatment",
                    cluster_var = "district")

sst_os_4 <- feols(entry_to_onscene_1 ~ number_sst_alerts + officer_hours +
                 number_dispatches_1 + number_dispatches_2 +
                 number_dispatches_3 + shotspot_border_treatment| district + date,
               cluster = ~district,
               data = dispatch_panel)

sst_os_5 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ number_sst_alerts  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)


sst_os_6 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ number_sst_alerts  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 | district + date,
        cluster = ~district)




# table -------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

intensive_table <- panelsummary_raw(list(sst_1, sst_2,
                      sst_3, sst_4, sst_5,
                      sst_6),
                 list(sst_os_1, sst_os_2,
                      sst_os_3, sst_os_4, sst_os_5,
                      sst_os_6),
                 stars = "econ",
                 mean_dependent = T,
                 coef_map = c( "number_sst_alerts" = "Number SST Alerts",
                               "shotspot_border_treatment" = "Border Police District"),
                 gof_omit = "^R|A|B|S",
                 gof_map = gof_mapping) 


intensive_table <- intensive_table %>% 
  janitor::clean_names() %>% 
  slice(-c(7:8)) %>% 
  mutate(model_3 = if_else(term == "FE: District" |
                             term == "FE: Day-by-Month-by-Year", "X", model_3)) %>% 
  mutate(model_3 = if_else(term == "Mean of Dependent Variable", 
                           model_2, model_3)) %>% 
  add_row(term = "Control Variables", model_1 = "", model_2 = "X", model_3 = "X", model_4 = "X",
          model_5 = "X", model_6 = "X") %>% 
  add_row(term = "Gardner (2021) Robust", model_1 = "", model_2 = "", model_3 = "X", model_4 = "",
          model_5 = "", model_6 = "") %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.double() %>% as.integer() %>% scales::comma(), .))) %>% 
  clean_raw(caption = "\\label{intensive_table}Effect of Number of ShotSpotter Alerts on Response Times (OLS)") %>% 
  pack_rows("Panel A: Call to Dispatch",1,6, italic = T, bold = F, hline_after = F) %>% 
  pack_rows("Panel B: Call to On-Scene", 7, 12, italic = T, bold = F,latex_gap_space = "0.5cm") %>% 
  row_spec(12, hline_after = TRUE) %>% 
  add_header_above(c(" " = 5, "> Median", "<= Median")) %>% 
  add_header_above(c(" " = 5,
                     "Officer Hours" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)


