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



onscene_3 <- dispatch_panel %>% 
  filter(priority_code == 3) %>% 
  mutate(across(c(entry_to_onscene, entry_to_dispatch), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call-to-Dispatch (Priority 3)` = entry_to_dispatch) +
                entry_to_dispatch_mins +
                (`Call-to-On-Scene (Priority 3)` = entry_to_onscene) +
                entry_to_onscene_mins
              ~ Mean + SD + Min  +Max + N,
               data = .,
               output = "data.frame")

onscene_2 <- dispatch_panel %>% 
  filter(priority_code == 2) %>% 
  mutate(across(c(entry_to_onscene, entry_to_dispatch), ~./60, .names = "{.col}_mins")) %>% 
  datasummary(
    (`Call-to-Dispatch (Priority 2)` = entry_to_dispatch) +
      entry_to_dispatch_mins +
      (`Call-to-On-Scene (Priority 2)` = entry_to_onscene) +
                entry_to_onscene_mins
              ~ Mean + SD + Min  +Max + N,
              data = .,
              output = "data.frame")

summary_stats_raw <- dispatch_panel_p1 %>% 
  mutate(across(c(entry_to_dispatch,
                  entry_to_onscene
  ), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call-to-Dispatch` = entry_to_dispatch) +
                entry_to_dispatch_mins +
                (`Call-to-On-Scene` = entry_to_onscene) +
                entry_to_onscene_mins + 
                (`Arrest Made` = arrest_made) +
                (`Priority 1 911 Dispatches` = number_dispatches) +
                (`ShotSpotter Dispatches` = number_sst_dispatches) +
                (`Officer Hours` = officer_hours) ~ Mean + SD + Min  +Max + N,
              data = .,
              output = "data.frame")

footnote <- map(list( "Units are in seconds unless otherwise noted. Data is at
         the call-level. Call-to-Dispatch represents 
         the amount of time from the 911 call to an officer dispatching
         to the scene. Call-to-On-Scene is the time from a 911 call to
         when an officer arrives on-scene.
         Priority 1 Call-to-On-Scene is missing approximately 45 percent
         of on-scene times, although the results
         remain consistent when we can observe both response times. This is discussed further in Appendix A.
         Arrest Made is and indicator equal to one if the 911
         call resulted in an arrest.
         Priority 1 refers to an immediate dispatch, 
         Priority 2 a rapid dispatch, and Priority 3 a routine dispatch.
         Priority 2 and 3 analysis, while not in the main text, is available
         in Appendix E.
         Priority 1 911 Dispatches is the number of Priority 1 dispatches at the
         district-day level.
         ShotSpotter Dispatches is the 
         number of dispatches due to ShotSpotter detections. Importantly, ShotSpotter Dispatches is
         also at the district-by-day level and includes days in which
         ShotSpotter is not implemented. The average number of ShotSpotter dispatches
         on post-implementation days is approximately 6. The average daily number of ShotSpotter dispatches across Chicago
         once all 12 districts have implemented ShotSpotter is approximately 70. Note that
         New Years Eve/New Years Day/Fourth of July are excluded from the sample as
         these days correspond with high amounts of celebratory gunfire. Officer Hours are the 
         number of working hours sworn police officers work at the district-day level.
                  "), ~str_remove_all(., "\n"))

summary_stats <- summary_stats_raw %>% 
  add_row(onscene_2, .before = 6) %>% 
  add_row(onscene_3, .before = 10) %>% 
  janitor::clean_names() %>% 
  mutate(across(.cols = c(-1), ~prettyNum(.,digits = 2, big.mark = ",", format = "f"))) %>% 
  mutate(x = if_else(str_detect(x, "mins$"),
                     "", x)) %>%
  mutate(across(c(-1), ~if_else(x == "", paste0("(", str_trim(.), " mins)"), .))) %>% 
  mutate(n = if_else(x == "", "", n)) %>% 
  kbl(col.names = c(" ", "Mean", "Std. Dev.", "Min", "Max", "N"),
      booktabs = T,
      caption = "\\label{summary_stats}Summary Statistics",
      format = "latex") %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 10) %>% 
  pack_rows(group_label = "Panel A: Priority 1 Outcomes:", 1, 5,
            italic = F, bold = T) %>% 
  pack_rows(group_label = "Panel B: Secondary Outcomes:", 6, 13,
            latex_gap_space = "0.5cm",
            italic = F, bold = T) %>% 
  pack_rows(group_label = "Panel C: Other Variables:", 14, 16,
            latex_gap_space = "0.3cm") %>% 
  footnote(footnote, threeparttable = T)

writeLines(summary_stats, "paper/tables/summary_stats.tex")
