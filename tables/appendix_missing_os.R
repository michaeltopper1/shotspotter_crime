library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)
library(modelsummary)

missing_onscene <- read_csv(here::here("analysis_data/xxmissing_onscene.csv"))

missing_onscene %>% 
  group_by(year_month ) %>% 
  summarize(portion_missing_os = mean(portion_missing_os)) %>% 
  ggplot(aes(year_month, portion_missing_os)) +
  geom_point() +
  geom_line() +
  labs(x = "", y = "Proportion Missing On-Scene Time") +
  theme_minimal()

missing_2s <- did2s(missing_onscene,
      yname = "portion_missing_os",
      first_stage = ~0|district + date,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district")

missing <- missing_onscene %>% 
  feols(portion_missing_os ~treatment |district + date)

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Dependent variable is the proportion of 911 call dispatches that 
                  have missing on-scene times. ShotSpotter Activated refers to the
                  timing in which each district receives ShotSpotter technology. The
                  Gardner (2022) estimator is robust to the heterogeneous
                  treatment effects in staggered two-way-fixed-effects designs."), ~str_remove_all(., "\n"))

missing_os_table <- panelsummary_raw(list(missing, missing_2s),
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
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.integer() %>%  scales::comma(), .))) %>% 
  clean_raw(caption = "\\label{missing_os_table}Proportion of Missing Call-to-On-Scene Data (OLS)") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)
