library(tidyverse)
library(fixest)

arrests <- readxl::read_excel('raw_data/arrests_database.xlsx',
                    sheet = "DataSet I") %>% janitor::clean_names()

rollout_dates <- read_csv("created_data/rollout_dates.csv")
rollout_dates <- rollout_dates %>% mutate(across(starts_with("shotspot"), ~mdy(.)))



arrests <- arrests %>%
  mutate(date = mdy_hms(arrestdate) %>% as_date(),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

arrests <- arrests %>% 
  mutate(district = parse_number(cpd_district)) 

## gettign rid of districts that have NA and districts above 25th

arrests <- arrests %>% 
  drop_na(district) %>% 
  filter(district <= 25)


arrests <- arrests %>% 
  mutate(firearm_arrest = if_else(str_detect(charge_description,"GUN|CARRY CONCL|CONCEAL|FIREARM"), 1, 0)) 

arrests_agg <- arrests %>% 
  group_by(date, district) %>% 
  summarize(total_arrests = n(),
            firearm_arrests = sum(firearm_arrest, na.rm = T)) %>% 
  ungroup()

arrests_agg <- arrests_agg %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), 
         treatment_official = if_else(shotspot_activate_official <= date, 1, 0),
         treatment_first_shot = if_else(shotspot_activate_first_shot <= date, 1, 0),
         treatment_cpd = if_else(shotspot_activate_cpd <= date, 1, 0),
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  treatment_official = if_else(is.na(treatment_official), 0, treatment_official),
  treatment_first_shot = if_else(is.na(treatment_first_shot), 0, treatment_first_shot),
  .by = district)


arrests_agg <- arrests_agg %>% 
  filter(date < as_date("2023-01-01"))

arrests_agg %>% 
  feols(total_arrests ~treatment | district + date)
  
arrests_agg %>% 
  feols(firearm_arrests ~treatment | district + date)


arrests_agg_es <- arrests_agg %>% 
  mutate(time_to_treat = time_length(as_date(date) - shotspot_activate,
                                     "month") %>% 
           magrittr::add(1) %>% 
           floor() %>%
           magrittr::subtract(1),
         .by = district) %>% 
  mutate(time_to_treat = case_when(
    time_to_treat > 24 ~ 24,
    time_to_treat < -12 ~ -12,
    .default = time_to_treat
  )) %>% 
  mutate(time_to_treat = if_else(is.na(time_to_treat), -1000, time_to_treat)) 

arrests_agg_es %>% 
  feols(firearm_arrests ~ i(time_to_treat, ref = c(-1, -1000)) |district + date,
        cluster = ~district, data = .) %>% 
  fixest::coefplot()
