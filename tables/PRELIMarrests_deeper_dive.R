library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}


## officers can name an action with several letters attached for miscellaneous events.

person_not_found_1 <- paste0(c(1:19), "B")
person_not_found_2 <- paste0(c(1:19), "BZ")
person_not_found <- c(person_not_found_1, person_not_found_2)

person_left_1 <- paste0(c(1:19), "E")
person_left_2 <- paste0(c(1:19), "EZ")
person_left <- c(person_left_1, person_left_2)

recontact_if_return_1 <- paste0(c(1:19), "H")
recontact_if_return_2 <- paste0(c(1:19), "HZ")
recontact_if_return <- c(recontact_if_return_1, recontact_if_return_2)

no_police_service_1 <- paste0(c(1:19), "D")
no_police_service_2 <- paste0(c(1:19), "DZ")
no_police_service <- c(no_police_service_1, no_police_service_2)

not_bonafide_1 <- paste0(c(1:19), "A")
not_bonafide_2 <- paste0(c(1:19), "AZ")
not_bonafide <- c(not_bonafide_1, not_bonafide_2)

other_police_1 <- paste0(c(1:19), "P")
other_police_2 <- paste0(c(1:19), "P")
other_police <- c(other_police_1, other_police_2)

slow_arrival <- c(person_not_found, person_left, no_police_service, not_bonafide)
person_left_or_not_found <- c(person_not_found,person_left)

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(slow_arrival = if_else(final_disposition_code %in% slow_arrival, 1, 0),
         person_left_or_not_found = if_else(final_disposition_code %in% person_left_or_not_found, 1, 0)) %>% 
  mutate(across(c(slow_arrival, person_left_or_not_found), ~ replace_na(., 0))) 

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(person_not_found = if_else(final_disposition_code %in% person_not_found, 1,0),
         person_left = if_else(final_disposition_code %in% person_left,1, 0),
         recontact = if_else(final_disposition_code %in% recontact_if_return,1, 0),
         no_police_service = if_else(final_disposition_code %in% person_left,1, 0),
         not_bonafide = if_else(final_disposition_code %in% not_bonafide,1, 0),
         other_police = if_else(final_disposition_code %in% other_police,1, 0)) %>% 
  mutate(across(c(person_not_found, person_left,
                  recontact, no_police_service,
                  not_bonafide, other_police), ~replace_na(.,0)))

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_code", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

## looks good
dispatch_panel_p1 %>% 
  filter(final_dispatch_description %in% c("DOMESTIC BATTERY", "DOMESTIC DISTURBANCE")) %>% 
  # count(final_disposition_description, final_disposition_code, sort = T)
  feols(arrest_made ~ treatment | date + district  + hour + final_dispatch_description,
        cluster = ~district) %>% 
  etable(fitstat = "my")
  panelsummary(stars = "econ",
               gof = gof_mapping) %>% 
  add_header_above(c(" ", "Person Not Found", "Person Left",
                     "Recontact if Return", "Not Bonafide", "Other Police"))





domestic_codes <- dispatch_panel_p1 %>% 
  filter(arrest_made == 0) %>% 
  count(final_disposition_description,
        final_disposition_code, sort = T) %>% 
  head(6) %>% 
  pull(final_disposition_code)





dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(person_not_found = if_else(final_disposition_code %in% person_not_found, 1, 0),
         person_left = if_else(final_disposition_code %in% person_left, 1, 0),
         advised_warrant = if_else(final_disposition_code %in% advised_warrant, 1, 0),
         recontact_if_return = if_else(final_disposition_code %in% recontact_if_return, 1, 0)) 

dispatch_panel_p1 %>% 
  count(person_not_found)

  count(recontact_if_return)
  summarize(mean(person_not_found, na.rm = T))
  count(person_not_found)

did2s(data = dispatch_panel_p1 %>% 
        filter(str_detect(final_dispatch_description, "IP|JO")),
      yname = "arrest_made",
      first_stage = ~0|district + date + 
        final_dispatch_code + hour,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district") %>% 
  etable(fitstat = "my")

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  select(-person_left_not_found) %>% 
  mutate(person_left_not_found = if_else(final_disposition_code %in% person_left_or_not_found, 1, 0)) 


dispatch_panel_p1 %>% 
  feglm(person_not_found ~ treatment | date + district + hour + final_dispatch_description,
        cluster = ~district, family = "logit")

dispatch_panel_p1 %>% 
  group_by(year = year(date)) %>% 
  summarize(person_not_found = sum(person_left_or_not_found, na.rm = T),
            person_left = sum(person_left, na.rm = T)) %>% 
  ggplot(aes(year, person_not_found)) +
  geom_line()

dispatch_panel_p1 %>% 
  # filter(final_dispatch_description == "DOMESTIC BATTERY") %>% 
  feols(person_not_found ~ treatment | date + district + hour + final_dispatch_description,
        cluster = ~district) 


dispatch_panel_p1 %>% 
  filter(arrest_made == 0) %>%
  # filter(final_dispatch_description == "DOMESTIC BATTERY") %>% 
  filter(final_dispatch_description == "BATTERY IP") %>% 
  count(person_not_found, person_left, not_bonafide) %>% 
  mutate(n/sum(n))
