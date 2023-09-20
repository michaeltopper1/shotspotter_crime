
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


arrest_rate <- dispatch_panel_p1 %>% 
  feglm(arrest_made ~ treatment + ..ctrl, data = .,
        family = "logit")

arrest_rate_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 1) %>% 
  feglm(arrest_made ~ treatment + ..ctrl, data = .,
        family = "logit")

arrest_rate_no_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report != 1) %>% 
  feglm(arrest_made ~ treatment + ..ctrl, data = .,
        family = "logit")

arrest_rate_domestic_bat <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "DOMESTIC BATTERY") %>% 
  feglm(arrest_made ~ treatment + ..ctrl, data = .,
        family = "logit")

arrest_rate_domestic_disturb <-  dispatch_panel_p1 %>%
  filter(final_dispatch_description == "DOMESTIC DISTURBANCE") %>% 
  feglm(arrest_made ~ treatment + ..ctrl, data = .,
        family = "logit")

arrest_rate_battery <-  dispatch_panel_p1 %>%
  filter(final_dispatch_description == "BATTERY IP") %>% 
  feglm(arrest_made ~ treatment + ..ctrl, data = .,
        family = "logit")

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. All estimations are using logit estimation.
                      The dependent variable is an indicator equal to one if a 911 call ended in an arrest.
                      Column 1 reports the pooled estimates using the entire sample.
                  Columns 2 and 3 subset Column 1 by gun-related and non-gun-related 911 calls.
                  Gun-related crimes are those corresponding to the following
                  911 code descriptions: `person with a gun',
                  `shots fired', or `person shot'. 
                  Columns 4-6 report the three most frequent 911 calls that end in arrest: Domestic Disturbance,
                  Domestic Battery, and Robbery. In some cases,
                  some observations may be dropped due to no variation
                  with certain fixed effects.
                  "), ~str_remove_all(., "\n"))

arrest_table_raw <-
  panelsummary_raw(
    list(
      arrest_rate,
      arrest_rate_gun,
      arrest_rate_no_gun,
      arrest_rate_domestic_bat,
      arrest_rate_domestic_disturb,
      arrest_rate_battery
    ),
    mean_dependent = T,
    stars = "econ",
    coef_map = c(
      "treatment" = "ShotSpotter Activated",
      "shotspot_border_treatment" = "Border Activated"
    ),
    gof_omit = "^R|A|B|S",
    fmt = 3,
    gof_map = gof_mapping
  ) 


arrest_prob_logit <- arrest_table_raw %>% 
  janitor::clean_names() %>% 
  clean_raw(pretty_num = T, format = 'latex',
            caption = "\\label{arrest_prob_logit}Effect of ShotSpotter Enactment on 911 Arrest Probability (Logit)") %>% 
  row_spec(4, hline_after = T) %>% 
  add_header_above(c(" " = 1,
                     "All" = 1,
                     "Gun" = 1,
                     "Non-Gun" = 1,
                     "Domestic\nDisturbance" = 1,
                     "Domestic\nBattery" =1,
                     "Robbery" = 1)) %>% 
  add_header_above(c(" " = 2,
                     "Gun-Relation" = 2,
                     "Most Frequent Arrest 911 Calls" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(arrest_prob_logit, "paper/appendix_tables/arrest_prob_logit.tex")
