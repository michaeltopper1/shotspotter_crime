## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-24
##

library(tidyverse)
library(modelsummary)
library(kableExtra)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}


# dispatch_panel %>% 
#   filter(never_treated == 0) %>% 
#   filter(date >= as_date("2018-05-16")) %>% 
#   group_by(date) %>% 
#   summarize(sst_per_day = sum(number_sst_alerts)) %>% 
#   summarize(average = mean(sst_per_day),
#             sd = sd(sst_per_day),
#             min = min(sst_per_day),
#             max = max(sst_per_day),
#             median = median(sst_per_day))


summary_stats <- dispatch_panel %>% 
  mutate(across(c(entry_to_dispatch_1,
                  entry_to_onscene_1
                  ), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call to Dispatch (Priority 1)` = entry_to_dispatch_1) +
                entry_to_dispatch_1_mins +
              (`Call to On-Scene (Priority 1)` = entry_to_onscene_1) +
              entry_to_onscene_1_mins +
              (`Number Dispatches` = number_dispatches)+
              (`Priority 1` = number_dispatches_1) +
              (`Priority 2` = number_dispatches_2) +
              (`Priority 3` = number_dispatches_3) +
              (`Number Arrests` = arrests_made) +
              (`Arrest Rate` = arrest_rate) +
              (`Number SST Alerts` = number_sst_alerts) +
              (`Officer Hours` = officer_hours) +
              (`Number Gun Victimizations` = num_any_gunshot_victim)~ Mean + SD + Median + Min  +Max,
              data = .,
              output = "data.frame") %>% 
  janitor::clean_names()



footnote <- map(list( "Units are in seconds unless otherwise noted. Data is at
         the district-by-day level. Call to Dispatch represents 
         the amount of time from the 911 call to the dispatcher finding and dispatching
         a police officer to the scene. Dispatch to On-Scene is the time from dispatch to 
         the scene of the reported crime. Priority 1 refers to an immediate dispatch, 
         Priority 2 a rapid dispatch, and Priority 3 a routine dispatch. Officer Hours are the 
         number of working hours sworn police officers work. Number of SST Alerts is the 
         number of ShotSpotter alerts. Note that
         New Years Eve/New Years Day/Fourth of July are excluded from the sample as
         ShotSpotter Alerts can be as high as 392 on these days. 
                  "), ~str_remove_all(., "\n"))


summary_stats <- summary_stats %>% 
  mutate(across(c(-1), ~. %>% as.double() %>% scales::comma(accuracy  = 0.01))) %>% 
  mutate(x = if_else(str_detect(x, "mins$"),
                     "", x)) %>% 
  mutate(across(c(-1), ~if_else(x == "", paste0("(", ., " mins)"), .))) %>% 
  kbl(col.names = c(" ", "Mean", "Std. Dev.", "Median", "Min", "Max"),
      booktabs = T,
      caption = "\\label{summary_stats}Summary Statistics of Response Times (seconds)") %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>% 
  pack_rows("Main Outcomes:", 1, 4,bold = T) %>% 
  pack_rows("Controls/Secondary Outcomes:", 5, 13, latex_gap_space = "0.2cm", bold = T) %>% 
  add_indent(c(6,7,8)) %>% 
  footnote(footnote, threeparttable = T)

# balance -----------------------------------------------------------------



# dispatch_panel %>% 
#   mutate(across(c(entry_to_dispatch_1,
#                   entry_to_onscene_1
#   ), ~./60, .names = "{.col}_mins")) %>% 
#   select(`Call to Dispatch (Priority 1)` = entry_to_dispatch_1,
#                 entry_to_dispatch_1_mins,
#                 `Call to On-Scene (Priority 1)` = entry_to_onscene_1,
#                 entry_to_onscene_1_mins,
#                 `Number Dispatches` = number_dispatches,
#                 `Priority 1` = number_dispatches_1,
#                 `Priority 2` = number_dispatches_2,
#                 `Priority 3` = number_dispatches_3,
#                 `Number SST Alerts` = number_sst_alerts,
#                 `Officer Hours` = officer_hours,
#                 `Number Arrests` = arrests_made,
#                 `Number Gun Victimizations` = number_gun_victims,
#          never_treated) %>% 
#   mutate(never_treated = if_else(never_treated == 0, "Treated Districts",
#                                  "Untreated Districts")) %>% 
#   datasummary_balance(~never_treated, data = ., output = "data.frame") %>% 
#   mutate(across(c(-1), ~. %>% as.double() %>% scales::comma(accuracy  = 0.01))) %>%
#   janitor::clean_names() %>% 
#   mutate(x = if_else(str_detect(x, "mins$"),
#                      "", x)) %>% 
#   kbl(col.names = c(" ", "Mean", "Std. Dev.", "Mean", "Std.Dev", "Diff in Means", "Std. Error"),
#       booktabs = T,
#       caption = "\\label{summary_stats}Summary Statistics of Response Times (seconds)") %>% 
#   kable_styling(latex_options = "HOLD_position") %>% 
#   pack_rows("Main Outcomes:", 1, 4) %>% 
#   pack_rows("Controls/Secondary Outcomes:", 5, 12) %>% 
#   add_indent(c(6,7,8)) %>% 
#   add_header_above(c(" " = 1, "Treated" = 2, "Never Treated" = 2, " " =2)) %>% 
#   footnote(footnote, threeparttable = T)

         