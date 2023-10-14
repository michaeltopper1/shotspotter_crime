
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

## need this for body worn camera implementation
bwc <- read_csv("created_data/bwc_rollout.csv") %>% 
  mutate(bwc_date = mdy(bwc_date),
         sdsc_max = mdy(sdsc_max))

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  left_join(bwc) %>% 
  mutate(treatment_bwc = if_else(date >= bwc_date, 1, 0),
         treatment_bwc = if_else(is.na(treatment_bwc), 0, treatment_bwc),
         treatment_sdsc = if_else(date >=sdsc_max, 1, 0),
         treatment_sdsc = if_else(is.na(treatment_sdsc), 0, treatment_sdsc),
         .by = district)


# Panel A: call to dispatch --------------------------------------------------------


sdsc_d1 <- dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~treatment + treatment_sdsc + ..ctrl)

sdsc_d1_2s <- did2s(data = dispatch_panel_p1,
                    yname = "entry_to_dispatch",
                    first_stage = ~treatment_sdsc + ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")

sdsc_d2 <- dispatch_panel_p1 %>% 
  filter(!district %in% c(7,9)) %>% 
  feols(entry_to_dispatch ~treatment + treatment_sdsc + ..ctrl)

sdsc_d2_2s <- did2s(data = dispatch_panel_p1 %>% 
                      filter(!district %in% c(7,9)),
                    yname = "entry_to_dispatch",
                    first_stage = ~treatment_sdsc + ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")

bwc_d1 <- dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~treatment + treatment_bwc + ..ctrl)

bwc_d1_2s <- did2s(data = dispatch_panel_p1,
                   yname = "entry_to_dispatch",
                   first_stage = ~treatment_bwc + ..ctrl,
                   second_stage = ~treatment,
                   treatment = "treatment",
                   cluster_var = "district")



# Panel B: call to on-scene --------------------------------------------------------


sdsc_o1 <- dispatch_panel_p1 %>% 
  feols(entry_to_onscene ~treatment + treatment_sdsc + ..ctrl)

sdsc_o1_2s <- did2s(data = dispatch_panel_p1,
                    yname = "entry_to_onscene",
                    first_stage = ~treatment_sdsc + ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")


sdsc_o2 <- dispatch_panel_p1 %>% 
  filter(!district %in% c(7,9)) %>% 
  feols(entry_to_onscene ~treatment + treatment_sdsc + ..ctrl)

sdsc_o2_2s <- did2s(data = dispatch_panel_p1 %>% 
                      filter(!district %in% c(7,9)),
                    yname = "entry_to_onscene",
                    first_stage = ~treatment_sdsc + ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")

bwc_o1 <- dispatch_panel_p1 %>% 
  feols(entry_to_onscene ~treatment + treatment_bwc + ..ctrl)

bwc_o1_2s <- did2s(data = dispatch_panel_p1,
                   yname = "entry_to_onscene",
                   first_stage = ~treatment_bwc + ..ctrl,
                   second_stage = ~treatment,
                   treatment = "treatment",
                   cluster_var = "district")



# table -------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_code", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01"), ~str_remove_all(., "\n"))





confounding_table_raw_dispatch <- panelsummary_raw(list(sdsc_d1, sdsc_d1_2s, sdsc_d2, sdsc_d2_2s, bwc_d1, bwc_d1_2s),
                                          stars = "econ",
                                          mean_dependent = T,
                                          coef_map = c("treatment" = "ShotSpotter Activated",
                                                       "treatment_sdsc" = "SDSC Activated",
                                                       "treatment_bwc" = "BWC Activated"),
                                          gof_omit = "^R|A|B|S",
                                          gof_map = gof_mapping)

confounding_table_raw_onscene <- panelsummary_raw(list(sdsc_o1, sdsc_o1_2s, sdsc_o2, sdsc_o2_2s, bwc_o1, bwc_o1_2s),
                                                   stars = "econ",
                                                   mean_dependent = T,
                                                   coef_map = c("treatment" = "ShotSpotter Activated",
                                                                "treatment_sdsc" = "SDSC Activated",
                                                                "treatment_bwc" = "BWC Activated"),
                                                   gof_omit = "^R|A|B|S",
                                                   gof_map = gof_mapping)

confounding_table_dispatch <- confounding_table_raw_dispatch %>% 
  slice(-c(9:11)) %>% 
  janitor::clean_names() %>% 
  mutate(model_4 = if_else(term == "Mean of Dependent Variable", 
                           model_3, model_4),
         model_2 = if_else(term == "Mean of Dependent Variable", 
                           model_1, model_2),
         model_6 = if_else(term == "Mean of Dependent Variable", 
                           model_5, model_6)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.006", model_2 = "",
          model_3 = "0.004", model_4 = "", model_5 = "0.010", model_6 = "", .before = 9) %>% 
  add_row(term = "Gardner (2021) Robust", model_1 = "", model_2 = "X", model_3 = "", model_4 = "X",
          model_5 = "", model_6 = "X") %>% 
  clean_raw(pretty_num =  T,
            format = "html") %>% 
  add_header_above(c(" " = 3, "Omitting Districts 7 and 9" = 2, " " = 2)) %>% 
  add_header_above(c(" " =1, "SDSC Controls" = 4, "BWC Controls" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")


confounding_table_onscene <- confounding_table_raw_onscene %>% 
  slice(-c(9:11)) %>% 
  janitor::clean_names() %>% 
  mutate(model_4 = if_else(term == "Mean of Dependent Variable", 
                           model_3, model_4),
         model_2 = if_else(term == "Mean of Dependent Variable", 
                           model_1, model_2),
         model_6 = if_else(term == "Mean of Dependent Variable", 
                           model_5, model_6)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.002", model_2 = "",
          model_3 = "0.001", model_4 = "", model_5 = "0.002", model_6 = "", .before = 9) %>% 
  add_row(term = "Gardner (2021) Robust", model_1 = "", model_2 = "X", model_3 = "", model_4 = "X",
          model_5 = "", model_6 = "X") %>% 
  clean_raw(pretty_num =  T,
            format = "html") %>% 
  add_header_above(c(" " = 3, "Omitting Districts 7 and 9" = 2, " " = 2)) %>% 
  add_header_above(c(" " =1, "SDSC Controls" = 4, "BWC Controls" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

write_file(confounding_table_dispatch, file = "presentations/tables/confounding_table_dispatch.html")

write_file(confounding_table_onscene, file = "presentations/tables/confounding_table_onscene.html")
