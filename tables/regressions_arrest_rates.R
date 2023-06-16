
library(modelsummary)

outcomes <- c("Overall Change",
              "Domestic Battery",
              "Domestic Disturbance",
              "Robbery",
              "EMS",
              "Battery")

arrest_rates <- feols(c(arrest_rate_1,
                        domestic_battery_p1_arrestrate,
                        domestic_disturb_p1_arrestrate,
                        robbery_jo_p1_arrestrate,
                        ems_p1_arrestrate,
                        battery_ip_p1_arrestrate) ~treatment  +
                        number_dispatches_1 + number_dispatches_2 +
                        number_dispatches_3 +
                        officer_hours|district + date,
                      data= dispatch_panel) 

arrest_means <- map_dfr(arrest_rates, ~as.numeric(fitstat(., type = "my"))) %>% 
  mutate(across(everything(), ~sprintf("%.3f",.))) %>% 
  mutate(term = "Mean of Dependent Variable")
  


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Panel A shows the time from entry call to dispatched officer.
                  Panel B shows time from the dispatched officer to on scene. Controls
                  in all models include controls for officer hours and number of dispatches.
                  "), ~str_remove_all(., "\n"))

arrest_rates %>% 
  panelsummary_raw(stars = "econ",
               coef_map = c( "treatment" = "ShotSpotter Activated",
                             "shotspot_border_treatment" = "Border Activated"),
               gof_omit = "^R|A|B|S",
               gof_map = gof_mapping) %>% 
  add_row(arrest_means, .before = 3) %>% 
  mutate(across(-c(term), ~if_else(term == "Observations",
                                   . %>% as.double() %>% as.integer() %>% scales::comma(), .))) %>% 
  clean_raw(caption = "\\label{arrest_rates}Effect of ShotSpotter Enactment on Arrest Rates (OLS)") %>% 
  add_header_above(c(" " = 1,
                     "Arrest Rate" = 1,
                     "Domestic Battery" = 1,
                     "Domestic Disturbance" = 1,
                     "Robbery" = 1,
                     "EMS" = 1,
                     "Battery" = 1)) %>% 
  add_header_above(c(" "= 2,
                     "Arrest Rate by Most Frequent Arrest Calls" = 5)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_paper()


