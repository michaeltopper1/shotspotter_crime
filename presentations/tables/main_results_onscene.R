
library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}



dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district)

## for wild cluster boot only
# dispatch_panel_p1 <- dispatch_panel_p1 %>% 
#   mutate(date = as.integer(date),
#          final_dispatch_code = match(final_dispatch_code, unique(final_dispatch_code)))

# entry to dispatch -------------------------------------------------------

os_d <- feols(entry_to_onscene ~ treatment | district + date,
                 cluster = ~district,
                 data = dispatch_panel_p1)

os_d_1 <- feols(entry_to_onscene ~ treatment | district + date +
                     hour + final_dispatch_description,
                   cluster = ~district,
                   data = dispatch_panel_p1)

os_d_2 <- feols(entry_to_onscene ~ treatment  +
                     officer_hours + number_dispatches| district + date +
                     final_dispatch_description + hour,
                   cluster = ~district,
                   data = dispatch_panel_p1)

os_d_3 <- did2s(data = dispatch_panel_p1,
                   yname = "entry_to_onscene",
                   first_stage = ~0|district + date + 
                     final_dispatch_description + hour,
                   second_stage = ~treatment,
                   treatment = "treatment",
                   cluster_var = "district")

os_d_4 <- feols(entry_to_onscene ~ treatment + shotspot_border_treatment| district + date +
                     final_dispatch_description + hour,
                   cluster = ~district,
                   data = dispatch_panel_p1)



footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors clustered by district. OLS estimates unless noted.
                      Border District Activated is binary for a police district adjacent 
                      to a ShotSpotter district."
), ~str_remove_all(., "\n"))

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

dispatch_raw_os <- panelsummary_raw(list(os_d, os_d_1,
                                      os_d_3, os_d_4),
                                 stars = "econ",
                                 mean_dependent = T,
                                 coef_map = c("treatment" = "ShotSpotter Activated",
                                              "shotspot_border_treatment" = "Border District Activated"),
                                 gof_omit = "^R|A|B|S",
                                 gof_map = gof_mapping) %>% 
  janitor::clean_names()

onscene_table <- dispatch_raw_os %>% 
  select(-model_1) %>% 
  mutate(model_3 = if_else(term == "FE: District" |
                             term == "FE: Day-by-Month-by-Year", "X", model_3)) %>%
  mutate(model_3 = if_else(term == "FE: Call-Type" |
                             term == "FE: Hour-of-Day", "X", model_3)) %>% 
  mutate(model_3 = if_else(term == "Mean of Dependent Variable", 
                           model_2, model_3)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_2 = "0.012",
            model_3 = "X", model_4 = "0.017", .before = 7) %>% 
  add_row(term = "Gardner (2021) Robust", model_2 = "No", model_3 = "Yes", model_4 = "No") %>% 
  add_row(term = "Clusters", model_2 = "22", model_3 = "22", model_4 = "22") %>% 
  slice(-c(8:11)) %>% 
  clean_raw(pretty_num = T,
            format = "html") %>% 
  row_spec(7, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Effect on Call-to-On-Scene (seconds)" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

onscene_table_c1 <- onscene_table %>% 
  column_spec(2, background = "yellow")
onscene_table_c2 <- onscene_table %>% 
  column_spec(3, background = "yellow")
onscene_table_c3 <- onscene_table %>% 
  column_spec(4, background = "yellow")

write_file(onscene_table, file = "presentations/tables/onscene_table.html")

write_file(onscene_table_c1, file = "presentations/tables/onscene_table_c1.html")
write_file(onscene_table_c2, file = "presentations/tables/onscene_table_c2.html")
write_file(onscene_table_c3, file = "presentations/tables/onscene_table_c3.html")
