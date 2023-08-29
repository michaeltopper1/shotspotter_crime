


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
  filter(time_sensitive_call == 1) %>% 
  feols(victim_injury ~ treatment + ..ctrl)

## time sensitive call with gun is person with a gun or shots fired
victim_gun <- dispatch_panel_p1 %>% 
  filter(time_sensitive_call_gun == 1) %>% 
  feols(victim_injury ~ treatment + ..ctrl)

## non-time sensitive stuff is everything else that can be put into the table
victim_no_gun <- dispatch_panel_p1 %>% 
  filter(time_sensitive_call_no_gun == 1) %>% 
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
                      The sample here is restricted to only Priority 1 dispatches that
                      are time-sensitive and have the possibility of an injury. For instance,
                      a dispatch for a person shot is not time sensitive since
                      the injury has already been realized. On the other
                      hand, a dispatch for a person with a knife is considered time-sensitive as
                      an injury has not yet occurred, but may occur if an officer arrives slower.
                      Gun Dispatch is restricted to only time-sensitive gun dispatches including
                      'Person with a Gun' and 'Shots Fired'. Non-Gun Dispatch are all other
                      time-sensitive dispatches. 
                  . 
                  "), ~str_remove_all(., "\n"))

victim_table <- panelsummary(list(victim_1, victim_gun, victim_no_gun),
             mean_dependent = T, stars = "econ",
             coef_map = c( "treatment" = "ShotSpotter Activated",
                           "shotspot_border_treatment" = "Border Activated"),
             gof_omit = "^R|A|B|S",
             fmt = 3,
             gof_map = gof_mapping,
             collapse_fe = T,
             pretty_num = T,
             caption = "\\label{victim_table}Effect of ShotSpotter Implementation on Victim Injury (OLS)",
             format = "latex") %>% 
  add_header_above(c(" " = 1,"Full Sample" = 1, "Gun Dispatch" = 1, "Non-Gun Dispatch" = 1)) %>% 
  add_header_above(c(" " = 1, "Probability of Victim Injury" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(victim_table, "paper/tables/victim_table.tex")
