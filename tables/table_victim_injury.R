


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
  feols(victim_injury ~ treatment + ..ctrl)

## time sensitive call with gun is person with a gun or shots fired
victim_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 1) %>% 
  feols(victim_injury ~ treatment + ..ctrl)

## non-time sensitive stuff is everything else that can be put into the table
victim_no_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 0) %>% 
  feols(victim_injury ~ treatment + ..ctrl)
    


# -------------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                      The main outcome variable is the probability of a victim being
                      injured.
                      The Pooled column refers to using the entire sample of time-sensitive Priority 1
                      dispatches.
                      Gun Dispatch is restricted to only gun-related dispatches including
                      'Person with a Gun', 'Person Shot', and 'Shots Fired'. Non-Gun Dispatch are all other
                      dispatches. In all columns the preferred specification is estimated using
                      OLS. Wild Cluster Bootstrap pvalues are obtained with 1000 iterations due to the relatively
                      low number of clusters (22).
                  
                  "), ~str_remove_all(., "\n"))

victim_table <- panelsummary_raw(list(victim_1, victim_gun, victim_no_gun),
             mean_dependent = T, stars = "econ",
             coef_map = c( "treatment" = "ShotSpotter Activated",
                           "shotspot_border_treatment" = "Border Activated"),
             gof_omit = "^R|A|B|S",
             fmt = 3,
             gof_map = gof_mapping) %>% 
  janitor::clean_names() %>% 
  add_row(term = "Wild Cluster Bootstrap P-Value", model_1 = '0.245', model_2 = '0.067',
          model_3 = '0.895', .before = 5) %>% 
  clean_raw(caption = "\\label{victim_table}Effect of ShotSpotter Implementation on Probablity of 911 Victim Injury (OLS)",
              pretty_num = T,
            format = "latex") %>% 
  add_header_above(c(" " = 1,"Pooled" = 1, "Gun Dispatch" = 1, "Non-Gun Dispatch" = 1)) %>% 
  add_header_above(c(" " = 1, "Probability of Victim Injury" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>% 
  column_spec(1, width = "8cm") %>% 
  row_spec(5, hline_after = T)

writeLines(victim_table, "paper/tables/victim_table.tex")
