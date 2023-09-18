library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)


rollout_dates <- read_csv("created_data/rollout_dates.csv")
bwc <- read_csv("created_data/bwc_rollout.csv") %>% 
  mutate(bwc_date = mdy(bwc_date),
         sdsc_max = mdy(sdsc_max))


footnotes <- map(list("This table shows
                      the implementation dates of ShotSpotter technology and
                      Strategic Decision Support Centers (SDSC). SDSCs are implemented
                      in similar, although not the same time period.
                      The Difference column shows the number of days between
                      the SDSC implementation and ShotSpotter activation. On
                      average, this is approximately
                      73 days in districts that have both ShotSpotter and an SDSC. SDSCs contain many 
                      police prediction softwares, however,
                      only Hunchlab, a location prediction software,
                      is implemented in conjuction with these as the others
                      had been previously used in Chicago prior to SDSCs. 
                      Hunchlab has been
                      found to only change patrolling behaviors in
                      districts 7 and 9 as discussed in Kapustin et al. (2022). Further
                      robustness of the results including SDSC implementation dates
                      as controls are shown in Appendix Table B2."), ~str_remove_all(., "\n"))

rollout_difference <- rollout_dates %>% 
  full_join(bwc) %>% 
  select(district, shotspot_activate, sdsc_max, bwc_date) %>% 
  mutate(across(starts_with("shotspot"), ~mdy(.))) %>% 
  mutate(difference = shotspot_activate - sdsc_max,
         difference_bwc = abs(shotspot_activate - bwc_date)) %>%
  mutate(across(everything(), ~as.character(.) %>% replace_na(""))) %>%
  mutate(difference = if_else(difference != "",glue::glue("{difference} days"), ""),
         difference_bwc = if_else(difference != "",glue::glue("{difference_bwc} days"), "")) %>% 
  kbl(booktabs = T,
      col.names = c("District", "ShotSpotter", "SDSC", "BWC", "Difference SDSC",
                    "Difference BWC"),
      caption = "\\label{rollout_difference}Implementation Dates of ShotSpotter/SDSC/BWC",
      format = "latex") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 10)

writeLines(rollout_difference, "paper/appendix_tables/rollout_difference.tex")
  
