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
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

  

# dispatch_panel %>% 
#   filter(never_treated == 0) %>% 
#   filter(date >= as_date("2018-05-16")) %>% 
#   group_by(date) %>% 
#   summarize(sst_per_day = sum(number_sst_dispatches)) %>% 
#   summarize(average = mean(sst_per_day),
#             sd = sd(sst_per_day),
#             min = min(sst_per_day),
#             max = max(sst_per_day),
#             median = median(sst_per_day))
# 
# dispatch_panel %>% 
#   filter(never_treated == 0) %>% 
#   filter(date >= as_date("2018-05-16")) %>% 
#   group_by(date, district) %>% 
#   summarize(sst_per_day = sum(number_sst_dispatches)) %>% 
#   summarize(mean(sst_per_day), sd(sst_per_day)) %>% 
#   pull() %>% 
#   round(2)

victim <- dispatch_panel_p1 %>% 
  filter(time_sensitive_call == 1) %>% 
  datasummary((`Victim Injury Probability` = victim_injury_time_sensitive_call)~
                Mean + SD + Median + Min  +Max,
              data = .,
              output = "data.frame")

onscene_3 <- dispatch_panel %>% 
  filter(priority_code == 3) %>% 
  mutate(across(c(entry_to_onscene), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call-to-On-Scene (Priority 3)` = entry_to_onscene) +
                entry_to_onscene_mins
              ~ Mean + SD + Median + Min  +Max,
               data = .,
               output = "data.frame")

onscene_2 <- dispatch_panel %>% 
  filter(priority_code == 2) %>% 
  mutate(across(c(entry_to_onscene), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call-to-On-Scene (Priority 2)` = entry_to_onscene) +
                entry_to_onscene_mins
              ~ Mean + SD + Median + Min  +Max,
              data = .,
              output = "data.frame")

summary_stats <- dispatch_panel_p1 %>% 
  mutate(across(c(entry_to_dispatch,
                  entry_to_onscene
  ), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call-to-On-Scene` = entry_to_onscene) +
                entry_to_onscene_mins + 
                (`Call-to-Dispatch` = entry_to_dispatch) +
                entry_to_dispatch_mins +
                (`Arrest Probability` = arrest_made) +
                (`Number Dispatches` = number_dispatches) +
                (`Number SST Dispatches` = number_sst_dispatches) +
                (`Officer Hours` = officer_hours) ~ Mean + SD + Median + Min  +Max,
              data = .,
              output = "data.frame")

footnote <- map(list( "Units are in seconds unless otherwise noted. Data is at
         the district-by-day level. Call-to-Dispatch represents 
         the amount of time from the 911 call to an officer dispatching
         to the scene. Call-to-On-Scene is the time from a 911 call to
         when an officer arrives on scene. Priority 1 refers to an immediate dispatch, 
         Priority 2 a rapid dispatch, and Priority 3 a routine dispatch. Officer Hours are the 
         number of working hours sworn police officers work. Number of SST Dispatches is the 
         number of dispatches due to ShotSpotter alerts. Importantly, Number of SST Dispatches is
         also at the district-by-day level and includes days in which
         ShotSpotter is not implemented. The average number of ShotSpotter dispatches across Chicago
         once all 12 districts have implemented ShotSpotter is approximately 60. Note that
         New Years Eve/New Years Day/Fourth of July are excluded from the sample as
         ShotSpotter alerts can be as high as 392 on these days. 
                  "), ~str_remove_all(., "\n"))

summary_stats <- summary_stats %>% 
  add_row(victim, .before = 6) %>% 
  add_row(onscene_2, .before = 7) %>% 
  add_row(onscene_3, .before = 9) %>% 
  janitor::clean_names() %>% 
  mutate(across(c(-1), ~. %>% as.double() %>% scales::comma(accuracy  = 0.01))) %>% 
  mutate(x = if_else(str_detect(x, "mins$"),
                     "", x)) %>% 
  mutate(across(c(-1), ~if_else(x == "", paste0("(", ., " mins)"), .))) %>% 
  kbl(col.names = c(" ", "Mean", "Std. Dev.", "Median", "Min", "Max"),
      booktabs = T,
      caption = "\\label{summary_stats}Summary Statistics") %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>% 
  pack_rows(group_label = "Priority 1 Outcomes:", 1, 6) %>% 
  pack_rows(group_label = "Secondary Outcomes/Controls:", 7, 13) %>% 
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
#                 `Number SST Alerts` = number_sst_dispatches,
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

         