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

dispatch_panel_p1 %>% 
  filter(time_sensitive_call == 1) %>% 
  filter(final_dispatch_description == "PERSON WITH A KNIFE") %>% 
  feols(victim_injury ~ treatment + ..ctrl, data = .)



dispatch_panel_p1 %>% 
  filter(time_sensitive_call == 1) %>% 
  count(final_dispatch_description, victim_injury, sort = T)










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