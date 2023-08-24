library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)


rollout_dates <- read_csv("created_data/rollout_dates.csv")

footnotes <- map(list("This table shows
                      the implementation dates of ShotSpotter technology and
                      Strategic Decision Support Centers (SDSC). SDSCs are implemented
                      in similar, although not the same time period.
                      The Difference column shows the number of days between
                      the SDSC implementation and ShotSpotter activation. On
                      average, this is approximately
                      73 days. SDSCs contain many police prediction softwares, however,
                      only Hunchlab, a location prediction software,
                      is implemented in conjuction with these. This software has been
                      found to only change patroling behaviors in
                      districts 7 and 9 as discussed in YENS PAPER. Further
                      robustness of the results including SDSC implementation dates
                      as controls are shown in Appendix Table BLANK."), ~str_remove_all(., "\n"))

rollout_difference <- rollout_dates %>% 
  left_join(bwc) %>% 
  select(district, shotspot_activate, sdsc) %>% 
  mutate(across(c(-1), ~mdy(.))) %>% 
  mutate(difference = shotspot_activate - sdsc) %>%
  kbl(booktabs = T,
      col.names = c("District", "ShotSpotter Activated", "SDSC Implemented", "Difference"),
      caption = "\\label{rollout_difference}Rollout Dates: ShotSpotter vs. SDSC") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 10)
  
