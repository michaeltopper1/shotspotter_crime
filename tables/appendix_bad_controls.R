

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

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)


# officer hours -----------------------------------------------------------

officer_hours <- dispatch_panel_p1 %>% 
  feols(officer_hours ~ treatment +..ctrl)

officer_hours_2s <- did2s(dispatch_panel_p1,
                    yname = "officer_hours",
                    first_stage = ~..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")


# number dispatches -------------------------------------------------------

number_dispatches <- dispatch_panel_p1 %>% 
  feols(number_dispatches ~ treatment +..ctrl)

number_dispatches_2s <- did2s(dispatch_panel_p1,
                          yname = "number_dispatches",
                          first_stage = ~..ctrl,
                          second_stage = ~treatment,
                          treatment = "treatment",
                          cluster_var = "district")



gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district.
                      This table shows estimations on two outcome variables, 
                      Number 911 Dispatches and Officer Hours, which are not
                      included in the main specification due to possibly being
                      bad controls.
                      Each panel refers to a distinct outcome variable.
                  Number 911 Dispatches is the number of 911 dispatches.
                  Officer Hours is the number of police officer hours.
                  ShotSpotter Activated refers to the
                  timing in which each district receives ShotSpotter technology. The
                  Gardner (2022) estimator is robust to the heterogeneous
                  treatment effects in staggered two-way-fixed-effects designs."), ~str_remove_all(., "\n"))

bad_controls <- panelsummary_raw(list(number_dispatches, number_dispatches_2s),
                                     list(officer_hours, officer_hours_2s),
             gof_map = gof_mapping,
             gof_omit = "^R|A|B|S",
             coef_map = c("treatment" = "ShotSpotter Activated"),
             mean_dependent = T,
             stars = T) %>% 
  mutate(`Model 2` = if_else(term == "Mean of Dependent Variable" |
                               term == "FE: Day-by-Month-by-Year" |
                               term == "FE: District" |
                               term == "FE: Call-Type" |
                               term == "FE: Hour-of-Day",
                             `Model 1`, `Model 2`)) %>% 
  add_row(term = "Gardner (2022) Robust",
          `Model 1` = "", `Model 2` = "X") %>% 
  slice(-c(5:8)) %>% 
  clean_raw(caption = "\\label{bad_controls}Bad Controls (OLS)",
            pretty_num = T) %>% 
  pack_rows("Panel A: Number 911 Dispatches",start_row = 1, end_row = 4,
            italic = T, bold = F) %>% 
  pack_rows("Panel B: Officer Hours", start_row = 5, end_row = 8,
            italic = T, bold = F) %>% 
  row_spec(8, hline_after = T) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)


# 
# missing_onscene %>% 
#   group_by(year_month ) %>% 
#   summarize(portion_missing_os = mean(portion_missing_os)) %>% 
#   ggplot(aes(year_month, portion_missing_os)) +
#   geom_point() +
#   geom_line() +
#   labs(x = "", y = "Proportion Missing On-Scene Time") +
#   theme_minimal()

