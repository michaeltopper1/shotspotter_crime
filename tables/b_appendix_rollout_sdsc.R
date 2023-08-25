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
  full_join(bwc) %>% 
  select(district, shotspot_activate, sdsc, bwc_date) %>% 
  mutate(across(starts_with("s"), ~mdy(.))) %>% 
  mutate(difference = shotspot_activate - sdsc,
         difference_bwc = abs(shotspot_activate - bwc_date)) %>%
  kbl(booktabs = T,
      col.names = c("District", "ShotSpotter", "SDSC", "BWC", "Difference SDSC",
                    "Difference BWC"),
      caption = "\\label{rollout_difference}Implementation Dates of ShotSpotter/SDSC/BWC") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 10)
  
