library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
  
}

dispatch_panel <- dispatch_panel %>% 
  rowwise() %>% 
  mutate(arrest_rate_1 = arrests_made_1/number_dispatches_1,
         arrest_rate_2 = arrests_made_2/number_dispatches_2,
         arrest_rate_3 = arrests_made_3/number_dispatches_3) %>% 
  ungroup()


a1 <- dispatch_panel %>% 
  feols(arrest_rate_1 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)

a1_r <- did2s(data = dispatch_panel,
              yname = "arrest_rate_1",
              first_stage = ~officer_hours + number_dispatches_1 +
                number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date,
              second_stage = ~treatment,
              treatment = "treatment",
              cluster_var = "district")

a2 <- dispatch_panel %>% 
  feols(arrest_rate_2 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)

a3 <- dispatch_panel %>% 
  feols(arrest_rate_3 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)

ar1 <- dispatch_panel %>% 
  feols(arrests_made_1 ~treatment + officer_hours + number_dispatches_1 +
          number_dispatches_2 + number_dispatches_3 + number_dispatches_0| district + date)


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01"), ~str_remove_all(., "\n"))


arrest_rate_table <- panelsummary::panelsummary_raw(list(a1,a1_r, a2, a3), 
                           mean_dependent = T, 
                           stars = "econ",
                           coef_map = c( "treatment" = "ShotSpotter Activated",
                                         "shotspot_border_treatment" = "Border Activated"),
                           gof_omit = "^R|A|B|S",
                          
                           gof_map = gof_mapping) %>% 
  mutate(`Model 2` = if_else(term == "FE: District" |
                               term == "FE: Day-by-Month-by-Year", "X", `Model 2`)) %>% 
  add_row(term = "Gardner (2021) Robust Estimator",
          `Model 1` = "",
          `Model 2` = "X",
          `Model 3` = "",
          `Model 4` = "") %>% 
  mutate(`Model 2` = if_else(term == "Mean of Dependent Variable", 
                             `Model 1`, `Model 2`)) %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.double() %>% as.integer() %>% scales::comma(), .))) %>% 
  clean_raw() %>% 
  add_header_above(c(" " =1, 
                     "Priority 1" = 2,
                     "Priority 2" = 1,
                     "Priority 3" = 1)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria")
