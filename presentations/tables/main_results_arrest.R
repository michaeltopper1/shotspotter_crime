
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


arrest_table_raw <- panelsummary_raw(list(arrest_rate, arrest_rate_gun, arrest_rate_no_gun, arrest_rate_domestic_bat,
                  arrest_rate_domestic_disturb, arrest_rate_battery
),
mean_dependent = T, stars = "econ",
coef_map = c( "treatment" = "ShotSpotter Activated",
              "shotspot_border_treatment" = "Border Activated"),
gof_omit = "^R|A|B|S",
fmt = 3,
gof_map = gof_mapping) %>% 
  slice(-c(5:8))

wild_bootstrap_arrest <- c('0.001', '0.412', '0.003', '0.049', 0.109)


arrest_table <- arrest_table_raw %>% 
  janitor::clean_names() %>% 
  add_row(term = "Wild Bootstrap P-Value",model_1 = "0.001", model_2 = "0.412",
          model_3 = "0.003", model_4 = "0.003", model_5 = "0.049", model_6 = "0.109", .before = 5) %>%
  clean_raw(pretty_num = T, format = 'html',
            caption = "Effect of ShotSpotter Enactment on Arrest Probability (OLS)") %>% 
  add_header_above(c(" " = 1,
                     "Pooled" = 1,
                     "Gun" = 1,
                     "Non-Gun" = 1,
                     "Domestic\nDisturbance" = 1,
                     "Domestic\nBattery" =1,
                     "Robbery" = 1)) %>% 
  add_header_above(c(" " = 2,
                     "Gun-Relation" = 2,
                     "Most Frequent Arrest Types" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

write_file(arrest_table, file = "presentations/tables/arrest_table.html")

