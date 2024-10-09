
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



# entry to dispatch -------------------------------------------------------


entry_d_1 <- feols(entry_to_dispatch ~ treatment | district + date +
                     hour + final_dispatch_description,
                   cluster = ~district,
                   data = dispatch_panel_p1)


entry_d_3 <- did2s(data = dispatch_panel_p1,
                   yname = "entry_to_dispatch",
                   first_stage = ~0|district + date + 
                     final_dispatch_description + hour,
                   second_stage = ~treatment,
                   treatment = "treatment",
                   cluster_var = "district")


# entry to dispatch; observed both ----------------------------------------

entry_d_1b <- feols(entry_to_dispatch ~ treatment | district + date +
                     hour + final_dispatch_description,
                   cluster = ~district,
                   data = dispatch_panel_p1 %>% drop_na(entry_to_onscene))


entry_d_3b <- did2s(data = dispatch_panel_p1 %>% 
                      drop_na(entry_to_onscene),
                   yname = "entry_to_dispatch",
                   first_stage = ~0|district + date + 
                     final_dispatch_description + hour,
                   second_stage = ~treatment,
                   treatment = "treatment",
                   cluster_var = "district")


# entry to dispatch -------------------------------------------------------


os_d_1 <- feols(entry_to_onscene ~ treatment | district + date +
                  hour + final_dispatch_description,
                cluster = ~district,
                data = dispatch_panel_p1)


os_d_3 <- did2s(data = dispatch_panel_p1,
                yname = "entry_to_onscene",
                first_stage = ~0|district + date + 
                  final_dispatch_description + hour,
                second_stage = ~treatment,
                treatment = "treatment",
                cluster_var = "district")



footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors clustered by district. Fixed effects include
                      hour-of-the-day, day-by-month-by-year,
                      district, and call-type. Sample includes only
                      Priority 1 911 dispatches. All columns use the
                      preferred specification outlined in Estimation Strategy slide."
), ~str_remove_all(., "\n"))

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

response_times_raw <- panelsummary_raw(list(entry_d_1, entry_d_3,
                                      os_d_1, os_d_3),
                                 stars = "econ",
                                 mean_dependent = T,
                                 coef_map = c("treatment" = "ShotSpotter Activated",
                                              "shotspot_border_treatment" = "Border District Activated"),
                                 gof_omit = "^R|A|B|S",
                                 gof_map = gof_mapping) %>% 
  janitor::clean_names()

response_time_table <- response_times_raw %>% 
  mutate(across(c(model_2, model_4), ~ if_else(term == "FE: District" |
                                                 term == "FE: Day-by-Month-by-Year", "X", .))) %>% 
  mutate(across(c(model_2, model_4), ~ if_else(term == "FE: Call-Type" |
                             term == "FE: Hour-of-Day", "X", .))) %>% 
  mutate(model_2 = if_else(term == "Mean of Dependent Variable", 
                           model_1, model_2)) %>% 
  mutate(model_4 = if_else(term == "Mean of Dependent Variable", 
                           model_3, model_4)) %>% 
  slice(c(1:4)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.018",
          model_2 = "X", model_3 = "0.007", model_4 = "X") %>% 
  # add_row(term = "Gardner (2021) Robust", model_2 = "No", model_3 = "Yes", model_4 = "No") %>% 
  add_row(term = "Clusters",model_1 = "22", model_2 = "22", model_3 = "22", model_4 = "22") %>% 
  clean_raw(pretty_num = T,
            format = "html") %>% 
  add_header_above(c(" " = 1, "OLS" = 1,
                     "Gardner (2021)" = 1,
                     "OLS" = 1,
                     "Gardner (2021)" = 1)) %>% 
  add_header_above(c(" " = 1, "Call-to-Dispatch" = 2,
                     "Call-to-On-Scene" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")



# creating table for observing both on-scene and dispatch -----------------

response_times_raw_b <- panelsummary_raw(list(entry_d_1b, entry_d_3b,
                                            os_d_1, os_d_3),
                                       stars = "econ",
                                       mean_dependent = T,
                                       coef_map = c("treatment" = "ShotSpotter Activated",
                                                    "shotspot_border_treatment" = "Border District Activated"),
                                       gof_omit = "^R|A|B|S",
                                       gof_map = gof_mapping) %>% 
  janitor::clean_names()

## remember to change boostrap
response_time_table_b <- response_times_raw_b %>% 
  mutate(across(c(model_2, model_4), ~ if_else(term == "FE: District" |
                                                 term == "FE: Day-by-Month-by-Year", "X", .))) %>% 
  mutate(across(c(model_2, model_4), ~ if_else(term == "FE: Call-Type" |
                                                 term == "FE: Hour-of-Day", "X", .))) %>% 
  mutate(model_2 = if_else(term == "Mean of Dependent Variable", 
                           model_1, model_2)) %>% 
  mutate(model_4 = if_else(term == "Mean of Dependent Variable", 
                           model_3, model_4)) %>% 
  slice(c(1:4)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.018",
          model_2 = "X", model_3 = "0.007", model_4 = "X") %>% 
  # add_row(term = "Gardner (2021) Robust", model_2 = "No", model_3 = "Yes", model_4 = "No") %>% 
  add_row(term = "Clusters",model_1 = "22", model_2 = "22", model_3 = "22", model_4 = "22") %>% 
  clean_raw(pretty_num = T,
            format = "html") %>% 
  add_header_above(c(" " = 1, "OLS" = 1,
                     "Gardner (2021)" = 1,
                     "OLS" = 1,
                     "Gardner (2021)" = 1)) %>% 
  add_header_above(c(" " = 1, "Call-to-Dispatch" = 2,
                     "Call-to-On-Scene" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")






response_time_table_col1 <- response_time_table %>% 
  column_spec(2, background = "yellow") 
response_time_table_col2 <- response_time_table %>% 
  column_spec(3, background = "yellow")
response_time_table_col3 <- response_time_table %>% 
  column_spec(4, background = "yellow") 
response_time_table_col4 <- response_time_table %>% 
  column_spec(5, background = "yellow") 



response_time_table_row_change <- response_time_table %>% 
  row_spec(4, background = "pink")
response_time_table_b_col <- response_time_table_b %>% 
  column_spec(2:3, background = "yellow") %>% 
  row_spec(4, background = "pink")


write_file(response_time_table, file = "presentations/tables/response_time_table.html")
write_file(response_time_table_col1, file = "presentations/tables/response_time_table_c1.html")
write_file(response_time_table_col2, file = "presentations/tables/response_time_table_c2.html")
write_file(response_time_table_col3, file = "presentations/tables/response_time_table_c3.html")
write_file(response_time_table_col4, file = "presentations/tables/response_time_table_c4.html")


write_file(response_time_table_row_change, file = "presentations/tables/response_time_table_r1.html")
write_file(response_time_table_b_col, file = "presentations/tables/response_time_table_r2.html")


