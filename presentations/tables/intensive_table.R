## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-17
##

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

officer_hours <- read_csv("analysis_data/officer_hours.csv")

rollout_dates <- read_csv("created_data/rollout_dates.csv") %>% 
  mutate(across(c(-1), ~mdy(.)))

sst_alerts <- read_csv("analysis_data/sst_dispatches_cpd.csv") %>% 
  filter(year < 2023) 


## need to do two analyses here: one for aggregate level data and one for
## call-level data (intensive margin)

# creating district-medians -----------------------------------------------

officer_hours_median <- officer_hours %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
  select(officer_hours_median, date, district)

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  left_join(officer_hours_median, by = join_by(district, date))


# aggregate for intensive margin ------------------------------------------

## aggregating to the district-day level
aggregate_outcomes <- dispatch_panel_p1 %>% 
  group_by(district, date) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene), ~ mean(.,na.rm = T))) %>% 
  ungroup()

## creating the treatment variables at the district-day level
aggregate_outcomes <- aggregate_outcomes %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), 
         treatment_sdsc = if_else(sdsc <= date, 1, 0 ),
         treatment_official = if_else(shotspot_activate_official <= date, 1, 0),
         treatment_first_shot = if_else(shotspot_activate_first_shot <= date, 1, 0),
         treatment_cpd = if_else(shotspot_activate_cpd <= date, 1, 0),
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  treatment_sdsc = if_else(is.na(treatment_sdsc), 0 , treatment_sdsc),
  treatment_official = if_else(is.na(treatment_official), 0, treatment_official),
  treatment_first_shot = if_else(is.na(treatment_first_shot), 0, treatment_first_shot),
  .by = district) 

## merging in the number of officer hours and officer hours median
aggregate_outcomes <- aggregate_outcomes %>% 
  left_join(officer_hours) %>% 
  left_join(officer_hours_median) %>% 
  left_join(sst_alerts,by = join_by(district, date))

## creating the number of sst dispatches variable for 
## variation 
aggregate_outcomes <- aggregate_outcomes %>%
  mutate(number_sst_dispatches = if_else(is.na(number_sst_dispatches), 0, number_sst_dispatches))



# Panel A -----------------------------------------------------------------

ed_preferred_1 <- feols(entry_to_dispatch ~ treatment | district + date +
                          final_dispatch_description + hour,
                        cluster = ~district,
                        data = dispatch_panel_p1)

sst_ed_1 <- feols(entry_to_dispatch ~ number_sst_dispatches | district + date,
                  cluster = ~district,
                  data = aggregate_outcomes %>% 
                    filter(treatment == 1 | never_treated == 1))

ed_above_med <- dispatch_panel_p1 %>%
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

## this ensures the median is after the restriction
sst_ed_above_med <- aggregate_outcomes %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_dispatch ~ number_sst_dispatches |district + date,
        cluster = ~district)

ed_below_med <- dispatch_panel_p1 %>%
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment |district + date,
        cluster = ~district)

sst_ed_below_med <- aggregate_outcomes %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_dispatch ~ number_sst_dispatches |district + date ,
        cluster = ~district)

# Panel B -----------------------------------------------------------------



os_preferred_1 <- feols(entry_to_onscene ~ treatment | district + date +
                          final_dispatch_description + hour,
                        cluster = ~district,
                        data = dispatch_panel_p1)

sst_os_1 <- feols(entry_to_onscene ~ number_sst_dispatches | district + date ,
                  cluster = ~district,
                  data = aggregate_outcomes %>% 
                    filter(treatment == 1 | never_treated == 1) )


os_above_med <- dispatch_panel_p1 %>%
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_onscene ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

sst_os_above_med <- aggregate_outcomes %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_onscene ~ number_sst_dispatches |district + date,
        cluster = ~district)

os_below_med <- dispatch_panel_p1 %>%
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_onscene ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)

sst_os_below_med <- aggregate_outcomes %>%
  filter(treatment == 1 | never_treated == 1) %>% 
  filter(officer_hours <= officer_hours_median) %>%
  feols(entry_to_onscene ~ number_sst_dispatches |district + date,
        cluster = ~district)





# table -------------------------------------------------------------------

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                      ShotSpotter Activated is a binary equal to one when
                      a district has ShotSpotter technology (extensive margin).
                      Number SST Dispatches refers to the number of
                      ShotSpotter dispatches that occur within a district-day (intensive margin).
                      All coefficient estimates are in seconds. The data is aggregated to the district-day level. The
                      coefficients of interest  are interpreted as marginal effects. We
                      aggregate to the district-day since the number of ShotSpotter dispatches is measured
                      at the district-day. 
                  "), ~str_remove_all(., "\n"))

intensive_table_raw <- panelsummary_raw(list(sst_ed_1,
                                             sst_os_1),
                                        stars = "econ",
                                        mean_dependent = T,
                                        coef_map = c("treatment" = "ShotSpotter Activated",
                                                     "number_sst_dispatches" = "Number SST Dispatches"),
                                        gof_omit = "^R|A|B|S",
                                        gof_map = gof_mapping) 


intensive_table <- intensive_table_raw %>% 
  janitor::clean_names() %>% 
  clean_raw(pretty_num = T,
            format = "html") %>% 
  add_header_above(c(" " = 1, "Call-to-Dispatch" = 1, "Call-to-On-Scene" = 1)) %>% 
  add_header_above(c(" " = 1,
                     "Intensive Margin (OLS)" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

intensive_table_col1 <- intensive_table %>% 
  column_spec(2, background = "yellow")
intensive_table_col2 <- intensive_table %>% 
  column_spec(3, background = "yellow")

write_file(intensive_table, file = "presentations/tables/intensive_table.html")
write_file(intensive_table_col1, file = "presentations/tables/intensive_table_col1.html")
write_file(intensive_table_col2, file = "presentations/tables/intensive_table_col2.html")

# 
# writeLines(mechanism_table, "paper/tables/mechanism_table.tex")


