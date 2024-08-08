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

## using this for percentages for 2sdid
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(arrest_made_p = arrest_made * 100)

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


# Panel C: Arrests --------------------------------------------------------


sdsc_a1 <- dispatch_panel_p1 %>% 
  feols(arrest_made*100 ~treatment + treatment_sdsc + ..ctrl)

sdsc_a1_2s <- did2s(data = dispatch_panel_p1,
                    yname = "arrest_made_p",
                    first_stage = ~treatment_sdsc + ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")


sdsc_a2 <- dispatch_panel_p1 %>% 
  filter(!district %in% c(7,9)) %>% 
  feols(arrest_made*100 ~treatment + treatment_sdsc + ..ctrl)

sdsc_a2_2s <- did2s(data = dispatch_panel_p1 %>% 
                      filter(!district %in% c(7,9)),
                    yname = "arrest_made_p",
                    first_stage = ~treatment_sdsc + ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")

bwc_a1 <- dispatch_panel_p1 %>% 
  feols(arrest_made*100 ~treatment + treatment_bwc + ..ctrl)

bwc_a1_2s <- did2s(data = dispatch_panel_p1,
                   yname = "arrest_made_p",
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

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district.
                      Coefficient estimates are in seconds for Panel A and B,
                      while estimates are in percent for Panel C.
                      Columns 1 and 2 of Panel A show
                       Call-to-Dispatch estimates when controlling for the implementation
                      of Strategic
                      Decision Support Centers (SDSC). In Columns 3 and 4, 
                      police districts 7 and 9 are omitted as Kapustin et al. (2022) shows that SDSCs 
                      affect
                      police patrolling in these districts. Panel B is similar to Panel A,
                      with the outcome of interest being Call-to-On-Scene times. In Panel C, the outcome of
                      interest is a binary for whether a 911 call ended in an arrest. In Columns 5 and 6,
                      we control for Body-Worn Camera (BWC) adoption. Note that in each specification,
                      controls are consistent with the preferred specification. OLS estimates are reported
                      in odd-numbered columns, while Gardner (2021) robust estimates are reported in even columns.
                      The coefficient estimates of controls when using Gardner (2021) estimator are not reported as the 
                      two-stage method only returns the coefficient estimate of interest on
                      the treated variable. In addition, the two-stage procedure may drop observations
                      in the first stage if unable to predict values. This happens infrequently as shown
                      in the observation counts, but is worth noting. Finally,
                      wild cluster bootstrap p-values using 999 iterations are also reported
                  as the number of clusters (22) is below the threshold of 30 put forth in
                  Cameron et al. (2008). The bootstrap procedure cannot be performed using the Gardner (2021) estimator.
                      
                   
                  "), ~str_remove_all(., "\n"))





confounding_table_raw <- panelsummary_raw(list(sdsc_d1, sdsc_d1_2s, sdsc_d2, sdsc_d2_2s, bwc_d1, bwc_d1_2s),
                 list(sdsc_o1, sdsc_o1_2s, sdsc_o2, sdsc_o2_2s, bwc_o1, bwc_o1_2s),
                 list(sdsc_a1, sdsc_a1_2s, sdsc_a2, sdsc_a2_2s, bwc_a1, bwc_a1_2s),
                 stars = "econ",
                 mean_dependent = T,
                 coef_map = c("treatment" = "ShotSpotter Activated",
                              "treatment_sdsc" = "SDSC Activated",
                              "treatment_bwc" = "BWC Activated"),
                 gof_omit = "^R|A|B|S",
                 gof_map = gof_mapping)

confounding_table <- confounding_table_raw %>% 
  slice(-c(9:11)) %>% 
  slice(-c(17:19)) %>% 
  slice(-c(25:27)) %>% 
  janitor::clean_names() %>% 
  mutate(model_4 = if_else(term == "Mean of Dependent Variable", 
                           model_3, model_4),
         model_2 = if_else(term == "Mean of Dependent Variable", 
                           model_1, model_2),
         model_6 = if_else(term == "Mean of Dependent Variable", 
                           model_5, model_6)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.006", model_2 = "",
          model_3 = "0.004", model_4 = "", model_5 = "0.010", model_6 = "", .before = 9) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.002", model_2 = "",
          model_3 = "0.001", model_4 = "", model_5 = "0.002", model_6 = "", .before = 18) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = "0.002", model_2 = "",
          model_3 = "0.009", model_4 = "", model_5 = "0.003", model_6 = "") %>% 
  add_row(term = "Gardner (2021) Robust", model_1 = "", model_2 = "X", model_3 = "", model_4 = "X",
          model_5 = "", model_6 = "X") %>% 
  clean_raw(pretty_num =  T, caption = "\\label{confounding_table}Robustness of Estimates Controlling for Other Technologies (OLS)",
            format = "latex") %>% 
  pack_rows("Panel A: Call-to-Dispatch", 1, 9, italic = T, bold = F) %>% 
  pack_rows("Panel B: Call-to-On-Scene", 10,18, italic = T, bold = F,
            latex_gap_space = "0.5cm") %>% 
  pack_rows("Panel C: Arrest Made", 19,27, italic = T, bold = F,
            latex_gap_space = "0.5cm") %>% 
  row_spec(27, hline_after = T) %>% 
  add_header_above(c(" " = 3, "Omitting Districts 7 and 9" = 2, " " = 2)) %>% 
  add_header_above(c(" " =1, "SDSC Controls" = 4, "BWC Controls" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 10)

writeLines(confounding_table, "paper/appendix_tables/confounding_table.tex")
