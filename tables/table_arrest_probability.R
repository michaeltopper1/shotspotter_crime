
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
  feols(arrest_made ~ treatment + ..ctrl, data = .)

arrest_rate_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 1) %>% 
  feols(arrest_made ~ treatment + ..ctrl, data = .)

arrest_rate_no_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report != 1) %>% 
  feols(arrest_made ~ treatment + ..ctrl, data = .)

arrest_rate_domestic_bat <- dispatch_panel_p1 %>%
  filter(final_dispatch_description == "DOMESTIC BATTERY") %>% 
  feols(arrest_made ~ treatment + ..ctrl)

arrest_rate_domestic_disturb <-  dispatch_panel_p1 %>%
  filter(final_dispatch_description == "DOMESTIC DISTURBANCE") %>% 
  feols(arrest_made ~ treatment + ..ctrl)

arrest_rate_battery <-  dispatch_panel_p1 %>%
  filter(final_dispatch_description == "BATTERY IP") %>% 
  feols(arrest_made ~ treatment + ..ctrl)




gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

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


arrest_prob <- panelsummary(list(arrest_rate, arrest_rate_gun, arrest_rate_no_gun, arrest_rate_domestic_bat,
                  arrest_rate_domestic_disturb, arrest_rate_battery
                  ),
             mean_dependent = T, stars = "econ",
             coef_map = c( "treatment" = "ShotSpotter Activated",
                           "shotspot_border_treatment" = "Border Activated"),
             gof_omit = "^R|A|B|S",
             fmt = 3,
             gof_map = gof_mapping,
             collapse_fe = T,
             pretty_num = T,
             caption = "\\label{arrest_prob}Effect of ShotSpotter Enactment on Arrest Probability (OLS)") %>% 
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
