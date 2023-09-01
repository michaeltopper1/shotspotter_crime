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


ed_above_med <- dispatch_panel_p1 %>%
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)


ed_below_med <- dispatch_panel_p1 %>%
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)


# Panel B -----------------------------------------------------------------



os_preferred_1 <- feols(entry_to_onscene ~ treatment | district + date +
                          final_dispatch_description + hour,
                        cluster = ~district,
                        data = dispatch_panel_p1)



os_above_med <- dispatch_panel_p1 %>%
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_onscene ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)


os_below_med <- dispatch_panel_p1 %>%
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_onscene ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)







# table -------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01"), ~str_remove_all(., "\n"))

mechanism_table_raw <- panelsummary_raw(list(ed_preferred_1,
                                             ed_above_med, ed_below_med
                                             ),
                                        list(os_preferred_1,
                                             os_above_med, os_below_med
                                             ),
                                        stars = "econ",
                                        mean_dependent = T,
                                        coef_map = c("treatment" = "ShotSpotter Activated",
                                                     "number_sst_dispatches" = "Number SST Dispatches"),
                                        gof_omit = "^R|A|B|S",
                                        gof_map = gof_mapping) 


mechanism_extensive <- mechanism_table_raw %>% 
  janitor::clean_names() %>% 
  slice(-c(5:8)) %>% 
  slice(-c(9:12)) %>% 
  clean_raw(colnames = c(" ","Pooled", "> Median", "<= Median"),
            pretty_num = T,
            caption = "Extensive Margin: Effect of ShotSpotter Implementation") %>% 
  pack_rows("Panel A: Call-to-Dispatch",1,4, italic = T, bold = F, hline_after = F) %>% 
  pack_rows("Panel B: Call-to-On-Scene", 5, 8, italic = T, bold = F) %>% 
  add_header_above(c(" " = 2,
                     "Officer Hours" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

write_file(mechanism_extensive, file = "presentations/tables/mechanism_extensive.html")

