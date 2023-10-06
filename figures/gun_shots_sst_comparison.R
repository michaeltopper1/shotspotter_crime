

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



sst <- read_csv("created_data/sst_dispatch_cpd.csv")

sst_counts <- sst %>% 
  mutate(date = as_date(entry_received_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year))) %>% 
  mutate(district = parse_number(district)) %>% 
  count(year_month, district) 


dispatch_panel_p1 %>% 
  filter(never_treated == 0) %>% 
  filter(final_dispatch_description == "SHOTS FIRED") %>% 
  filter(treatment == 0) %>% 
  count(date) %>% 
  summarize(mean(n))

dispatch_panel_p1 %>% 
  filter(never_treated == 0) %>% 
  filter(final_dispatch_description == "SHOTS FIRED") %>% 
  filter(treatment == 1) %>% 
  count(date) %>% 
  summarize(mean(n))

dispatch_panel_p1 %>% 
  filter(never_treated == 0) %>% 
  filter(final_dispatch_description == "SHOTS FIRED") %>% 
  filter(gun_crime_report == 1) %>% 
  count(year_month, district) %>% 
  ggplot(aes(year_month, n)) +
  geom_line() +
  geom_line(data = sst_counts, aes(), color = "red")  +
  facet_wrap(~district) +
  theme_minimal()
