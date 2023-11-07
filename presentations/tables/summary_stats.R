library(tidyverse)
library(modelsummary)
library(kableExtra)
library(panelsummary)


if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}


summary_stats <- dispatch_panel_p1 %>% 
  mutate(across(c(entry_to_dispatch,
                  entry_to_onscene
  ), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call-to-Dispatch (seconds)` = entry_to_dispatch) +
                entry_to_dispatch_mins +
                (`Call-to-On-Scene (seconds)` = entry_to_onscene) +
                entry_to_onscene_mins~ Mean + SD + Min  +Max + N,
              data = .,
              output = "data.frame") %>% 
  janitor::clean_names() %>% 
  mutate(x = if_else(str_detect(x, "mins$"),
                     "", x)) %>%
  mutate(across(c(-1), ~if_else(x == "", paste0("{", str_trim(.), " mins}"), .))) %>%
  mutate(n = if_else(x == "", "", n)) %>% 
  clean_raw(pretty_num = T,
            colnames = c(" ",
                         "Mean", "Std.Dev.","Min", "Max", "N"),
            format = "html") %>% 
  kable_styling(full_width = T, html_font = "Cambria")

write_file(summary_stats, file = "presentations/tables/summary_stats.html")
