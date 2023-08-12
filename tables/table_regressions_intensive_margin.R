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


sst_1 <- feols(entry_to_dispatch_1 ~ number_sst_dispatches | district + date,
                 cluster = ~district,
                 data = dispatch_panel)

sst_2 <- feols(entry_to_dispatch_1 ~ number_sst_dispatches + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 + number_dispatches_0| district + date,
                 cluster = ~district,
                 data = dispatch_panel)


sst_3 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ number_sst_dispatches + officer_hours + 
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3+ number_dispatches_0 | district + date,
        cluster = ~district)


sst_4 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_dispatch_1 ~ number_sst_dispatches  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3+ number_dispatches_0 | district + date,
        cluster = ~district)




# entry to onscene --------------------------------------------------------


sst_os_1 <- feols(entry_to_onscene_1 ~ number_sst_dispatches | district + date,
                    cluster = ~district,
                    data = dispatch_panel)

sst_os_2 <- feols(entry_to_onscene_1 ~ number_sst_dispatches + officer_hours +
                      number_dispatches_1 + number_dispatches_2 +
                      number_dispatches_3+ number_dispatches_0 | district + date,
                    cluster = ~district,
                    data = dispatch_panel)

sst_os_3 <- dispatch_panel %>% 
  filter(officer_hours > officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ number_sst_dispatches  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 + number_dispatches_0| district + date,
        cluster = ~district)


sst_os_4 <- dispatch_panel %>% 
  filter(officer_hours <= officer_hours_median) %>% 
  feols(entry_to_onscene_1 ~ number_sst_dispatches  + officer_hours +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3+ number_dispatches_0 | district + date,
        cluster = ~district)




# table -------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                      Number SST Dispatches refers to the number of
                      ShotSpotter dispatches that occur within a district-day.
                      All coefficient estimates are in seconds. Panel A reports results for
                      Call-to-Dispatch while Panel B reports results for Call-to-On-Scene.
                      Call-to-Dispatch is the amount of time from a 911 call to 
                      when a police officer is dispatched to the scene of the crime.
                      Call-to-On-Scene is the time from a 911 call to the time a police
                      officer arrives on-scene. In Column 1, the controls of officer hours and number of
                      911 dispatches are not included. Column 2 shows the preferred
                      specification, while Columns 3 and 4 split the sample by median number
                      of officer hours
                      within districts to show that response times are driven by
                      resource-constrained time periods. Observations for Call-to-On-Scene
                      do not exactly match Call-to-Dispatch since there is one district-day
                      that is missing information for Call-to-On-Scene. 
                  "), ~str_remove_all(., "\n"))

intensive_table <- panelsummary_raw(list(sst_1, sst_2,
                      sst_3, sst_4),
                 list(sst_os_1, sst_os_2,
                      sst_os_3, sst_os_4),
                 stars = "econ",
                 mean_dependent = T,
                 coef_map = c( "number_sst_dispatches" = "Number SST Dispatches"),
                 gof_omit = "^R|A|B|S",
                 gof_map = gof_mapping) 


intensive_table <- intensive_table %>% 
  janitor::clean_names() %>% 
  slice(-c(5:6)) %>% 
  add_row(term = "Control Variables", model_1 = "", model_2 = "X", model_3 = "X", model_4 = "X") %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% prettyNum(digits = 2, big.mark = ",", format = "f"), .))) %>% 
  clean_raw(caption = "\\label{intensive_table}Effect of Number of ShotSpotter Alerts on Response Times (OLS)") %>% 
  pack_rows("Panel A: Call-to-Dispatch",1,4, italic = T, bold = F, hline_after = F) %>% 
  pack_rows("Panel B: Call-to-On-Scene", 5, 8, italic = T, bold = F,latex_gap_space = "0.5cm") %>% 
  row_spec(8, hline_after = TRUE) %>% 
  add_header_above(c(" " = 3, "> Median", "<= Median")) %>% 
  add_header_above(c(" " = 3,
                     "Officer Hours" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)


