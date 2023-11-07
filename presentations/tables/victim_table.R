


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


## full sample of time-sensitive calls
victim_1 <- dispatch_panel_p1 %>% 
  feols(victim_injury*100 ~ treatment + ..ctrl)

## time sensitive call with gun is person with a gun or shots fired
victim_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 1) %>% 
  feols(victim_injury*100 ~ treatment + ..ctrl)

## non-time sensitive stuff is everything else that can be put into the table
victim_no_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 0) %>% 
  feols(victim_injury*100~ treatment + ..ctrl)



# -------------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Victim injury is a binary equal to one if 
                      a 911 call ends with a victim being injured. "), ~str_remove_all(., "\n"))

victim_table_raw <- panelsummary_raw(list(victim_1, victim_gun, victim_no_gun),
                             mean_dependent = T, stars = "econ",
                             coef_map = c( "treatment" = "ShotSpotter Activated",
                                           "shotspot_border_treatment" = "Border Activated"),
                             gof_omit = "^R|A|B|S",
                             gof_map = gof_mapping
                             ) 


wild_boot_values <- c('0.245','0.067','0.895')
victim_table <- victim_table_raw %>%
  slice(-c(5:8)) %>% 
  janitor::clean_names() %>% 
  add_row(term = "Wild Bootstrap P-Value", model_1 = '0.123', model_2 = '0.114',
          model_3 = '0.751') %>% 
  add_row(term = "Clusters", model_1 = '22', model_2 = '22',
          model_3 = '22') %>% 
  clean_raw(pretty_num = T,
            format = "html") %>% 
  add_header_above(c(" " = 1,"Pooled" = 1, "Gun Related" = 1, "Non-Gun Related" = 1)) %>% 
  add_header_above(c(" " = 1, "Likelihood of Victim Injury (percent)" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")


victim_table_pooled <- victim_table %>% 
  column_spec(2, background = "yellow")
victim_table_gun <- victim_table %>% 
  column_spec(3, background = "yellow") %>% 
  column_spec(4, background = "pink")


write_file(victim_table, file = "presentations/tables/victim_table.html")
write_file(victim_table_pooled, file = "presentations/tables/victim_table_pooled.html")

write_file(victim_table_gun, file = "presentations/tables/victim_table_gun.html")

