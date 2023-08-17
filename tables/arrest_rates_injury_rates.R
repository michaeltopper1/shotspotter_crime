
library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)


if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}


setFixest_fml(..ctrl = ~officer_hours +
                number_dispatches_1 + number_dispatches_2 + 
                number_dispatches_3 + number_dispatches_0| district + date)


arrest_rate <- dispatch_panel %>% 
  feols(arrest_rate_1 ~ treatment + ..ctrl, data = .)

arrest_rate_gun <- dispatch_panel %>% 
  feols(gun_crime_arrestrate_1 ~ treatment + ..ctrl, data = .)

arrest_rate_no_gun <- dispatch_panel %>% 
  feols(non_gun_crime_arrestrate_1 ~ treatment + ..ctrl, data = .)

arrest_rate_domestic_bat <- dispatch_panel %>% 
  feols(domestic_battery_p1_arrestrate ~ treatment + ..ctrl)

arrest_rate_domestic_disturb <-  dispatch_panel %>% 
  feols(domestic_disturb_p1_arrestrate ~ treatment + ..ctrl)

arrest_rate_battery <-  dispatch_panel %>% 
  feols(battery_ip_p1_arrestrate ~ treatment + ..ctrl)

victim_injury <- dispatch_panel %>% 
  feols(prob_victim_injury_1 ~ treatment + ..ctrl, data = .)

victim_injury_gun <- dispatch_panel %>% 
  feols(prob_victim_injury_guncrime_1 ~ treatment + ..ctrl, data = .)

victim_injury_no_gun <- dispatch_panel %>% 
  feols(prob_victim_injury_no_guncrime_1 ~ treatment + ..ctrl, data = .)



gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Panel A shows Arrest Rate defined as the number of arrests made
                  divided by the number of dispatches, while Panel B shows Injury defined as the
                  number of injury-related dispatches divided by the number of dispatches that are time-sensitive (see Appendix Figure BLANK). 
                  Columns 2 and 3 subset Column 1 by gun-related and non-gun-related arrest rates and injury rates.
                  Gun-related crimes for Arrest Rate are those corresponding to a person with a gun,
                  shots fired, or a person shot. Gun-related crimes to Injury Rate corresponds to
                  person with gun or shots fired.
                  Columns 3-5 report the 
                  top 3 most frequent calls that end in arrests: Domestic Battery,
                  Domestic Disturbance, and Battery. Observations are not consistent across each
                  call type since not every type of call occurs on every district-day. Controls
                  of officer hours and number of dispatches
                  are included in all specifications. 
                  "), ~str_remove_all(., "\n"))


panelsummary(list(arrest_rate, arrest_rate_gun, arrest_rate_no_gun, arrest_rate_domestic_bat,
                  arrest_rate_domestic_disturb, arrest_rate_battery
                  ), list(victim_injury, victim_injury_gun, victim_injury_no_gun),
             mean_dependent = T, stars = "econ",
             panel_labels = c("Panel A: Arrest Rate",
                              "Panel B: Injury Rate"),
             coef_map = c( "treatment" = "ShotSpotter Activated",
                           "shotspot_border_treatment" = "Border Activated"),
             gof_omit = "^R|A|B|S",
             fmt = 3,
             gof_map = gof_mapping,
             collapse_fe = T,
             pretty_num = T,
             caption = "\\label{arrest_rates}Effect of ShotSpotter Enactment on Arrest/Injury Rates (OLS)") %>% 
  add_header_above(c(" " = 1,
                     "All" = 1,
                     "Gun-Related" = 1,
                     "Non-Gun-Related" = 1,
                     "Domestic Disturbance" = 1,
                     "Domestic Battery" =1,
                     "Robbery" = 1)) %>% 
  add_header_above(c(" " = 4,
                     "Most Frequent Arrest Types" = 3)) %>% 
  column_spec(c(2:7), width = "30em") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)
