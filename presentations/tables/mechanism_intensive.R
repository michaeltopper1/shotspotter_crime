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




sst_ed_1 <- feols(entry_to_dispatch ~ number_sst_dispatches | district + date +
                    final_dispatch_description + hour,
                  cluster = ~district,
                  data = dispatch_panel_p1 %>% 
                    filter(treatment == 1 | never_treated == 1))

sst_ed_above_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_dispatch ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

sst_ed_below_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_dispatch ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)



sst_os_1 <- feols(entry_to_onscene ~ number_sst_dispatches | district + date +
                    final_dispatch_description + hour,
                  cluster = ~district,
                  data = dispatch_panel_p1 %>% 
                    filter(treatment == 1 | never_treated == 1) )

sst_os_above_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_onscene ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

sst_os_below_med <- dispatch_panel_p1 %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_onscene ~ number_sst_dispatches |district + date +
          final_dispatch_description + hour,
        cluster = ~district)


footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01"), ~str_remove_all(., "\n"))

mechanism_intensive_raw <- panelsummary_raw(list(sst_ed_1,
                                             sst_ed_above_med, sst_ed_below_med),
                                        list(sst_os_1, sst_os_above_med, sst_os_below_med),
                                        stars = "econ",
                                        mean_dependent = T,
                                        coef_map = c("treatment" = "ShotSpotter Activated",
                                                     "number_sst_dispatches" = "Number SST Dispatches"),
                                        gof_omit = "^R|A|B|S",
                                        gof_map = gof_mapping) 

mechanism_intensive <- mechanism_intensive_raw %>% 
  janitor::clean_names() %>% 
  slice(-c(5:8)) %>% 
  slice(-c(9:12)) %>% 
  clean_raw(colnames = c(" ","Pooled", "> Median", "<= Median"),
            pretty_num = T,
            caption = "Intensive Margin: Effect of Number of SST Dispatches") %>% 
  pack_rows("Panel A: Call-to-Dispatch",1,4, italic = T, bold = F, hline_after = F) %>% 
  pack_rows("Panel B: Call-to-On-Scene", 5, 8, italic = T, bold = F) %>% 
  add_header_above(c(" " = 2,
                     "Officer Hours" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

write_file(mechanism_intensive, file = "presentations/tables/mechanism_intensive.html")

