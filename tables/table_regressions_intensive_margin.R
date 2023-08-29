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


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district)


# Panel A -----------------------------------------------------------------

ed_preferred_1 <- feols(entry_to_dispatch ~ treatment | district + date +
                          final_dispatch_description + hour,
                        cluster = ~district,
                        data = dispatch_panel_p1)

sst_ed_1 <- feols(entry_to_dispatch ~ number_sst_dispatches | district + date +
                    final_dispatch_description + hour,
                  cluster = ~district,
                  data = dispatch_panel_p1 %>% 
                    filter(treatment == 1 | never_treated == 1))

ed_above_med <- dispatch_panel_p1 %>%
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

## this ensures the median is after the restriction
sst_ed_above_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_dispatch ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

ed_below_med <- dispatch_panel_p1 %>%
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

sst_ed_below_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_dispatch ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

# Panel B -----------------------------------------------------------------



os_preferred_1 <- feols(entry_to_onscene ~ treatment | district + date +
                          final_dispatch_description + hour,
                      cluster = ~district,
                      data = dispatch_panel_p1)

sst_os_1 <- feols(entry_to_onscene ~ number_sst_dispatches | district + date +
                    final_dispatch_description + hour,
                  cluster = ~district,
                  data = dispatch_panel_p1 %>% 
                    filter(treatment == 1 | never_treated == 1) )


os_above_med <- dispatch_panel_p1 %>%
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_onscene ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

sst_os_above_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_onscene ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

os_below_med <- dispatch_panel_p1 %>%
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_onscene ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

sst_os_below_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_onscene ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)





# table -------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

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

mechanism_table_raw <- panelsummary_raw(list(ed_preferred_1,
                                         ed_above_med, ed_below_med,
                                         sst_ed_1, sst_ed_above_med, sst_ed_below_med),
                      list(os_preferred_1,
                      os_above_med, os_below_med,sst_os_1,
                      sst_os_above_med, sst_os_below_med),
                 stars = "econ",
                 mean_dependent = T,
                 coef_map = c("treatment" = "ShotSpotter Activated",
                   "number_sst_dispatches" = "Number SST Dispatches"),
                 gof_omit = "^R|A|B|S",
                 gof_map = gof_mapping) 


mechanism_table <- mechanism_table_raw %>% 
  janitor::clean_names() %>% 
  slice(-c(7:10)) %>% 
  clean_raw(caption = "\\label{mechanism_table}Effect of ShotSpotter on Response Times Mechanisms (OLS)",
            pretty_num = T,
            format = "latex") %>% 
  pack_rows("Panel A: Call-to-Dispatch",1,6, italic = T, bold = F, hline_after = F) %>% 
  pack_rows("Panel B: Call-to-On-Scene", 7, 12, italic = T, bold = F,latex_gap_space = "0.5cm") %>% 
  row_spec(12, hline_after = TRUE) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 1, "> Median" = 1, "<= Median" = 1,
                     "Full Sample" = 1, "> Median" = 1, "<= Median" = 1)) %>% 
  add_header_above(c(" " = 2,
                     "Officer Hours" = 2,
                     " " = 1,
                     "Officer Hours" = 2)) %>% 
  add_header_above(c(" " =1,
                     "ShotSpotter Rollout" = 3,
                     "ShotSpotter Dispatches" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(mechanism_table, "paper/tables/mechanism_table.tex")
