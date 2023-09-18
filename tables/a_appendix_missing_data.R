

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

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)


# creating missing data for on scene indicator ----------------------------

dispatch_panel <- dispatch_panel %>% 
  mutate(missing_os = if_else(is.na(on_scene_date), 1, 0)) 

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(missing_os = if_else(is.na(on_scene_date), 1, 0))


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district)


# overall missing on scene data -------------------------------------------

missing <- dispatch_panel_p1 %>% 
  feols(missing_os ~ treatment + ..ctrl)

missing_above <- dispatch_panel_p1 %>% 
  filter(officer_hours > officer_hours_median) %>%
  feols(missing_os ~ treatment + ..ctrl)

missing_below <- dispatch_panel_p1 %>% 
  filter(officer_hours <= officer_hours_median) %>%
  feols(missing_os ~ treatment + ..ctrl)


# call to dispatch --------------------------------------------------------


dispatch_missing <- dispatch_panel_p1 %>%
  feols(entry_to_dispatch ~ treatment*missing_os| district^missing_os + date^missing_os + 
          final_dispatch_description^missing_os + hour^missing_os)

dispatch_missing_above <- dispatch_panel_p1 %>%
  filter(officer_hours >officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment*missing_os| district^missing_os + date^missing_os + 
          final_dispatch_description^missing_os + hour^missing_os)

dispatch_missing_below <- dispatch_panel_p1 %>%
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment*missing_os| district^missing_os + date^missing_os + 
          final_dispatch_description^missing_os + hour^missing_os)


# table -------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. All coefficient
                      estimates are in seconds. In Panel A,
                      the table shows regressions on a binary variable equal to
                      one if Call-to-On-Scene is missing. Columns 2 and 3 are split by
                      district-day medians of officer hours. In Panel B, Call-to-Dispatch
                      time, which contains no missing data, is estimated with an additional interaction term
                      which interacts Call-to-Dispatch time with the indicator for whether
                      on-scene time is missing. The coefficient estimate on this term shows that
                      that there is no difference in Call-to-Dispatch time when there is 
                      missing on-scene data. Note that in these specifications, the fixed effects
                      are also interacted to get a similar interpretation as if there were two separate
                      regressions estimated. All controls utilized in these regressions
                      are consistent with the preferred specification and are estimated
                      using OLS.
                  "), ~str_remove_all(., "\n"))



missing_table_raw <- panelsummary_raw(list(missing, missing_above, missing_below),
             list(dispatch_missing, dispatch_missing_above, dispatch_missing_below),
             stars = "econ",
             mean_dependent = T,
             coef_map = c("treatment" = "ShotSpotter Activated",
                          "missing" = "Missing",
                          "treatment:missing_os" = "ShotSpotter Activated x Missing",
                          "number_sst_dispatches" = "Number SST Dispatches"),
             gof_omit = "^R|A|B|S",
             gof_map = gof_mapping) 

missing_table <- missing_table_raw %>% 
  slice(-c(5:8)) %>% 
  clean_raw(pretty_num =T, 
            caption = "\\label{missing_data}Analysis of Missing Call-to-On-Scene Data (OLS)",
            format = "latex") %>% 
  pack_rows("Panel A: Missing Call-to-On-Scene", 1, 4,
            bold = F, italic = T) %>% 
  pack_rows("Panel B: Call-to-Dispatch", 5, 10,
            bold = F, italic = T) %>% 
  add_header_above(c(" " = 1, "Pooled" = 1, "> Median", "<= Median")) %>% 
  add_header_above(c(" " = 2, "Officer Availability" = 2)) %>% 
  column_spec(1, width = "8cm") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(missing_table, "paper/appendix_tables/missing_table.tex")
