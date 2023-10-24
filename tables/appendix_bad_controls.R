

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

## rolloutdates are for the treatment of SST
rollout_dates <- read_csv("created_data/rollout_dates.csv") %>% 
  mutate(across(c(-1), ~mdy(.)))

officer_hours <- read_csv("analysis_data/officer_hours.csv")


# creating panel ----------------------------------------------------------

panel_dates <- seq(as_date("2016-01-01"), as_date("2022-12-31") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)

# aggregating -------------------------------------------------------------


dispatch_panel_aggregate <- dispatch_panel %>% 
  group_by(date, district) %>% 
  summarize(number_dispatches = n()) %>% ungroup()


dispatch_panel_aggregate <- dispatch_panel_aggregate %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), 
         treatment_sdsc = if_else(sdsc <= date, 1, 0 ),
         treatment_official = if_else(shotspot_activate_official <= date, 1, 0),
         treatment_first_shot = if_else(shotspot_activate_first_shot <= date, 1, 0),
         treatment_cpd = if_else(shotspot_activate_cpd <= date, 1, 0),
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  treatment_sdsc = if_else(is.na(treatment_sdsc), 0 , treatment_sdsc),
  treatment_official = if_else(is.na(treatment_official), 0, treatment_official),
  treatment_first_shot = if_else(is.na(treatment_first_shot), 0, treatment_first_shot),
  .by = district)

dispatch_panel_aggregate <- dispatch_panel_aggregate %>% 
  left_join(officer_hours)

# officer hours -----------------------------------------------------------

officer_hours <- dispatch_panel_aggregate %>% 
  feols(officer_hours ~ treatment |district + date)

officer_hours_2s <- did2s(dispatch_panel_aggregate,
                    yname = "officer_hours",
                    first_stage = ~0|district + date,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district")


# number dispatches -------------------------------------------------------

number_dispatches <- dispatch_panel_aggregate %>% 
  feols(number_dispatches ~ treatment |district + date)

number_dispatches_2s <- did2s(dispatch_panel_aggregate,
                          yname = "number_dispatches",
                          first_stage = ~0|district + date,
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
                      "Standard errors are clustered by district. Coefficient
                      estimates are reported in seconds.
                      This table shows estimations on two outcome variables, 
                      Number of 911 Dispatches and Officer Availability, which are not
                      included in the main specification due to the possibility of being
                      confounding controls.
                      Each panel refers to a distinct outcome variable. Since each outcome variable
                      is at the district-day level, we aggregate the call-level data to the district-day.
                      Hence, in these models, we cannot control for call-type nor hour of the day.
                  Number 911 Dispatches is the number of 911 dispatches.
                  Officer Availability is the number of police officer hours within a district.
                  ShotSpotter Activated refers to the
                  timing in which each district receives ShotSpotter technology. The
                  Gardner (2021) estimator is robust to the heterogeneous
                  treatment effects in staggered two-way-fixed-effects designs. January 1, July 4, and December 31 are omitted due
                  to their correspondance with potential celebratory gunfire."), ~str_remove_all(., "\n"))

bad_controls <- panelsummary_raw(list(number_dispatches, number_dispatches_2s),
                                     list(officer_hours, officer_hours_2s),
             gof_map = gof_mapping,
             gof_omit = "^R|A|B|S",
             coef_map = c("treatment" = "ShotSpotter Activated"),
             mean_dependent = T,
             stars = "econ") %>% 
  mutate(`Model 2` = if_else(term == "Mean of Dependent Variable" |
                               term == "FE: Day-by-Month-by-Year" |
                               term == "FE: District" |
                               term == "FE: Call-Type" |
                               term == "FE: Hour-of-Day",
                             `Model 1`, `Model 2`)) %>% 
  add_row(term = "Gardner (2021) Robust",
          `Model 1` = "", `Model 2` = "X") %>% 
  slice(-c(5:6)) %>% 
  clean_raw(caption = "\\label{bad_controls}Effect of ShotSpotter Implementation on Confounding Controls (OLS)",
            pretty_num = T, format = "latex") %>% 
  pack_rows("Panel A: Number 911 Dispatches",start_row = 1, end_row = 4,
            italic = T, bold = F) %>% 
  pack_rows("Panel B: Officer Availability", start_row = 5, end_row = 8,
            italic = T, bold = F,
            latex_gap_space = "0.5cm") %>% 
  row_spec(8, hline_after = T) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>%
  column_spec(1, width = "8cm")

writeLines(bad_controls, "paper/appendix_tables/bad_controls.tex")

# 
# missing_onscene %>% 
#   group_by(year_month ) %>% 
#   summarize(portion_missing_os = mean(portion_missing_os)) %>% 
#   ggplot(aes(year_month, portion_missing_os)) +
#   geom_point() +
#   geom_line() +
#   labs(x = "", y = "Proportion Missing On-Scene Time") +
#   theme_minimal()

