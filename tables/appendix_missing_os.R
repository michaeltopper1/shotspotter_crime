library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)
library(modelsummary)

missing_onscene <- read_csv(here::here("analysis_data/xxmissing_onscene.csv"))

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}



# missing data ------------------------------------------------------------

missing <- missing_onscene %>% 
  feols(portion_missing_os ~treatment |district + date)

missing_2s <- did2s(missing_onscene,
      yname = "portion_missing_os",
      first_stage = ~0|district + date,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district")

missing_onscene %>% 
  filter(year != 2020) %>% 
  feols(portion_missing_os ~treatment |district + date)

# officer hours -----------------------------------------------------------

officer_hours <- dispatch_panel %>% 
  feols(officer_hours ~ treatment | district + date)

officer_hours_2s <- did2s(dispatch_panel,
                    yname = "officer_hours",
                    first_stage = ~0|district + date,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")


# number dispatches -------------------------------------------------------

number_dispatches <- dispatch_panel %>% 
  feols(number_dispatches ~ treatment | district + date)

number_dispatches_2s <- did2s(dispatch_panel,
                          yname = "number_dispatches",
                          first_stage = ~0|district + date,
                          second_stage = ~treatment,
                          treatment = "treatment",
                          cluster_var = "district")



gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district.
                      Each panel refers to a distinct outcome variable.
                  Missing Call-to-On-Scene is the proportion of 911 call dispatches that 
                  have missing on-scene times. 
                  Number Dispatches is the number of 911 dispatches.
                  Officer Hours is the number of police officer hours.
                  ShotSpotter Activated refers to the
                  timing in which each district receives ShotSpotter technology. The
                  Gardner (2022) estimator is robust to the heterogeneous
                  treatment effects in staggered two-way-fixed-effects designs."), ~str_remove_all(., "\n"))

missing_os_table <- panelsummary_raw(list(missing, missing_2s),
                                     list(number_dispatches, number_dispatches_2s),
                                     list(officer_hours, officer_hours_2s),
             gof_map = gof_mapping,
             gof_omit = "^R|A|B|S",
             coef_map = c("treatment" = "ShotSpotter Activated"),
             mean_dependent = T,
             stars = T) %>% 
  mutate(`Model 2` = if_else(term == "Mean of Dependent Variable" |
                               term == "FE: Day-by-Month-by-Year" |
                               term == "FE: District",
                             `Model 1`, `Model 2`)) %>% 
  add_row(term = "Gardner (2022) Robust",
          `Model 1` = "", `Model 2` = "X") %>% 
  slice(-c(5:6)) %>% 
  slice(-c(9,10)) %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.integer() %>%  scales::comma(), .))) %>% 
  clean_raw(caption = "\\label{missing_os_table}Proportion of Missing Call-to-On-Scene Data (OLS)") %>% 
  pack_rows("Panel A: Missing Call-to-On-Scene",start_row = 1, end_row = 4,
            italic = T, bold = F) %>% 
  pack_rows("Panel B: Number Dispatches", start_row = 5, end_row = 8,
            italic = T, bold = F) %>% 
  pack_rows("Panel C: Officer Hours", start_row = 9, end_row = 12,
            italic = T, bold = F) %>% 
  row_spec(12, hline_after = T) %>% 
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

