library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)


dispatch_panel <- read_csv("analysis_data/xxdispatch_panel.csv")


dispatch_panel <- dispatch_panel %>% 
  rowwise() %>% 
  mutate(arrest_rate_1 = arrests_made_1/number_dispatches_1,
         arrest_rate_2 = arrests_made_2/number_dispatches_2,
         arrest_rate_3 = arrests_made_3/number_dispatches_3) %>% 
  ungroup()


a1 <- dispatch_panel %>% 
  feols(arrest_rate_1 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)

a2 <- dispatch_panel %>% 
  feols(arrest_rate_2 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)

a3 <- dispatch_panel %>% 
  feols(arrest_rate_3 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)

ar1 <- dispatch_panel %>% 
  feols(arrests_made_1 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)


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


arrest_rate_table <- panelsummary::panelsummary(list(a1, a2, a3), 
                           mean_dependent = T, 
                           stars = "econ",
                           coef_map = c( "treatment" = "ShotSpotter Activated",
                                         "shotspot_border_treatment" = "Border Activated"),
                           gof_omit = "^R|A|B|S",
                           caption = "\\label{arrest_rate_table}Effect of ShotSpotter Rollout on Arrest Rate (OLS)",
                           gof_map = gof_mapping) %>% 
  add_header_above(c(" " =1, 
                     "Priority 1" = 1,
                     "Priority 2" = 1,
                     "Priority 3" = 1)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_paper()
