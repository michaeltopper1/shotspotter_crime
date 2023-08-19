## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-14
##

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



# entry to dispatch -------------------------------------------------------

entry_d <- feols(entry_to_dispatch ~ treatment | district + date,
                  cluster = ~district,
                  data = dispatch_panel_p1)

entry_d_1 <- feols(entry_to_dispatch ~ treatment | district + date +
                      final_dispatch_code + hour,
                    cluster = ~district,
                    data = dispatch_panel_p1)

entry_d_2 <- feols(entry_to_dispatch ~ treatment  +
                      officer_hours + number_dispatches| district + date +
                      final_dispatch_code + hour,
                    cluster = ~district,
                    data = dispatch_panel_p1)

entry_d_3 <- did2s(data = dispatch_panel_p1,
                    yname = "entry_to_dispatch",
                    first_stage = ~0|district + date + 
                      final_dispatch_code + hour,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")

entry_d_4 <- feols(entry_to_dispatch ~ treatment + shotspot_border_treatment| district + date +
                      final_dispatch_code + hour,
                    cluster = ~district,
                    data = dispatch_panel_p1)






# entry to onscene --------------------------------------------------------

entry_os <- feols(entry_to_onscene ~ treatment | district + date,
                  cluster = ~district,
                  data = dispatch_panel_p1)

entry_os_1 <- feols(entry_to_onscene ~ treatment | district + date +
                      final_dispatch_code + hour,
                 cluster = ~district,
                 data = dispatch_panel_p1)

entry_os_2 <- feols(entry_to_onscene ~ treatment  +
                      officer_hours + number_dispatches| district + date +
                      final_dispatch_code + hour,
                 cluster = ~district,
                 data = dispatch_panel_p1)

entry_os_3 <- did2s(data = dispatch_panel_p1,
                 yname = "entry_to_onscene",
                 first_stage = ~0|district + date + 
                   final_dispatch_code + hour,
                 second_stage = ~treatment,
                 treatment = "treatment",
                 cluster_var = "district")

entry_os_4 <- feols(entry_to_onscene ~ treatment + shotspot_border_treatment| district + date +
                      final_dispatch_code + hour,
                    cluster = ~district,
                    data = dispatch_panel_p1)




# -------------------------------------------------------------------------


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_code", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Panel A shows results for Call-to-Dispatch while
                  Panel B shows results for Call-to-On-Scene. Column 1 reports no controls, and 
                  only fixed effects. Controls in all other columns include
                  officer hours and number of 911 dispatches. Column 2 reports the preferred
                  specification from Equation 1. Column 3 reports estimates using
                  the Gardner (2022) estimator which is robust to 
                  heterogeneous treatment effects across groups and time periods
                  in staggered designs. Column 4 includes Border District Activated
                  which is an indicator for when a police district is adjacent to a ShotSpotter
                  implemented district. Wild cluster bootstrap p-values are also reported
                  as the number of clusters (22) is below the threshold of 30 put forth in
                  Cameron et al. (2008). Columns 5 and 6 split the sample by district median
                  levels of officer hours. Observations for Call-to-On-Scene
                      do not exactly match Call-to-Dispatch since there is one district-day
                      that is missing information for Call-to-On-Scene. 
                  "), ~str_remove_all(., "\n"))


dispatch_table <- panelsummary_raw(list(entry_d, entry_d_1,  entry_d_2,
                                        entry_d_3, entry_d_4),
  list(entry_os, entry_os_1,  entry_os_2,
                                    entry_os_3, entry_os_4),
                               stars = "econ",
                               mean_dependent = T,
                               coef_map = c("treatment" = "ShotSpotter Activated",
                            
                                            "shotspot_border_treatment" = "Border District Activated"),
                               gof_omit = "^R|A|B|S",
                               gof_map = gof_mapping) 

main_results <- dispatch_table %>% 
  janitor::clean_names() %>% 
  slice(-c(7:10)) %>% 
  mutate(model_4 = if_else(term == "FE: District" |
                               term == "FE: Day-by-Month-by-Year", "X", model_4)) %>%
  mutate(model_4 = if_else(term == "FE: Call-Type" |
                             term == "FE: Hour-of-Day", "X", model_4)) %>% 
  mutate(model_4 = if_else(term == "Mean of Dependent Variable", 
                             model_2, model_4)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.008", model_2 = "0.003",
          model_3 = "", model_4 = "", model_5 = "0.062", .before = 7) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.008", model_2 = "0.003",
          model_3 = "", model_4 = "", model_5 = "0.062", .before = 14) %>% 
  add_row(term = "Officer Hours", model_1 = "", model_2 = "", model_3 = "X", model_4 = "",
          model_5 = "") %>% 
  add_row(term = "Number 911 Dispatches", model_1 = "", model_2 = "", model_3 = "X", model_4 = "",
          model_5 = "") %>% 
  add_row(term = "Gardner (2022) Robust", model_1 = "", model_2 = "", model_3 = "", model_4 = "X",
          model_5 = "") %>% 
  mutate(across(c(2:6), ~prettyNum(.,digits = 2, big.mark = ",", format = "f"))) %>% 
  mutate(across(tidyselect::where(is.character), ~stringr::str_replace(., pattern = "NA", replacement = ""))) %>% 
  clean_raw(caption = "\\label{main_results}Effect of ShotSpotter on Response Times (OLS)",
            pretty_num = F) %>% 
  pack_rows("Panel A: Call-to-Dispatch", 1, 7, italic = T, bold = F) %>% 
  pack_rows("Panel B: Call-to-On-Scene", 8,14, italic = T, bold = F) %>% 
  row_spec(14, hline_after = TRUE) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)



