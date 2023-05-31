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


dispatch_panel <- read_csv("analysis_data/xxdispatch_panel.csv")


# panel A -----------------------------------------------------------------

## Entry to dispatch
entry_1 <- feols(entry_to_dispatch_1 ~ treatment | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

entry_2 <- feols(entry_to_dispatch_1 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district,
        data = dispatch_panel)

entry_3 <- dispatch_panel %>% 
  feols(entry_to_dispatch_1 ~ treatment + shotspot_border_treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)


entry_4 <- dispatch_panel %>% 
  feols(entry_to_dispatch_2~ treatment + shotspot_border_treatment +officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date)

entry_5 <- dispatch_panel %>% 
  feols(entry_to_dispatch_3~ treatment  +  shotspot_border_treatment +officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)





# entry to onscene --------------------------------------------------------


entry_os_1 <- feols(entry_to_onscene_1 ~ treatment | district + date,
                    cluster = ~district,
                    data = dispatch_panel)

entry_os_2 <- feols(entry_to_onscene_1 ~ treatment + officer_hours +
                      number_dispatches_1 + number_dispatches_2 +
                      number_dispatches_3 + number_dispatches_0| district + date,
                    cluster = ~district,
                    data = dispatch_panel)

entry_os_3 <- dispatch_panel %>% 
  feols(entry_to_onscene_1~ treatment + shotspot_border_treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)


entry_os_4 <- dispatch_panel %>% 
  feols(entry_to_onscene_2 ~ treatment + shotspot_border_treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date)

entry_os_5 <- dispatch_panel %>% 
  feols(entry_to_onscene_3~ treatment  +  shotspot_border_treatment +officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)


# -------------------------------------------------------------------------


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Priority calls range from 0-5. Only priorities 0-3 are
                  shown here as. Priority 0 is highest priority (office calling for help),
                  Priority 1 is immediate dispatch. Priority 2 is rapid dispatch. Priority 3
                  is routine dispatch. Panel A shows the time from entry call to dispatched officer.
                  Panel B shows time from the dispatched officer to on scene. Controls
                  in all models include controls for officer hours and number of dispatches.
                  "), ~str_remove_all(., "\n"))

dispatch_table <- panelsummary(list(entry_1, entry_2,
                                    entry_3, entry_4, entry_5),
                               list(entry_os_1, entry_os_2,
                                    entry_os_3, entry_os_4, entry_os_5),
                               stars = "econ",
                               panel_labels = c("Panel A: Call to Dispatch",
                                                "Panel B: Call to On-Scene"),
                               mean_dependent = T,
                               coef_map = c( "treatment" = "ShotSpotter Activated",
                                             "shotspot_border_treatment" = "Border Activated"),
                               gof_omit = "^R|A|B|S",
                               caption = "\\label{dispatch_table}Effect of ShotSpotter Rollout on Response Times (OLS)",
                               gof_map = gof_mapping,
                               italic  = T,
                               collapse_fe = T) %>% 
  add_header_above(c(" " = 4,
                     "Priority 2" = 1,
                     "Priority 3" = 1)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_paper()


