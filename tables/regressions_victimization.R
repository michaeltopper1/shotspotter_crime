## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-14
##

library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

victim_panel <- read_csv("analysis_data/xxvictim_panel.csv")
border_districts <- read_csv("created_data/border_districts_final.csv")

victim_panel <- victim_panel %>% 
  left_join(border_districts, join_by(district == border_district)) %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1 ,0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))





# panel A -----------------------------------------------------------------

ols_v1 <- victim_panel %>% 
  feols(number_gun_injury_victims ~treatment + officer_hours +
           number_gun_involved + number_other_crime | district + date)

ols_v2 <- victim_panel %>% 
  fepois(number_gun_injury_victims ~treatment + officer_hours + number_gun_involved
         + number_other_crime + shotspot_border_treatment| district + date)

poisson_v1 <- victim_panel %>% 
  fepois(number_gun_injury_victims ~treatment + officer_hours + number_gun_involved
         + number_other_crime| district + date)

poisson_v2 <- victim_panel %>% 
  fepois(number_gun_injury_victims ~treatment + officer_hours + number_gun_involved
         + number_other_crime + shotspot_border_treatment| district + date)


did2s(victim_panel,
      yname = "number_gun_injury_victims",
      first_stage = ~ officer_hours +
        number_gun_involved + number_other_crime + shotspot_border_treatment| district + date,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district")


# panel B -----------------------------------------------------------------



ols_prob_1 <- victim_panel %>% 
  feols(gun_injury_occur ~treatment + officer_hours + number_gun_involved
         + number_other_crime| district + date)

ols_prob_2 <- victim_panel %>% 
  feols(gun_injury_occur ~treatment + officer_hours + number_gun_involved
         + number_other_crime + shotspot_border_treatment| district + date)


logit_1 <- victim_panel %>% 
  feglm(gun_injury_occur ~treatment + officer_hours +
          number_gun_involved + number_other_crime| district + date ,
        family = binomial(link = "logit"))
logit_2 <- victim_panel %>% 
  feglm(gun_injury_occur ~treatment + officer_hours +
                   number_gun_involved + number_other_crime + shotspot_border_treatment| district + date ,
                 family = binomial(link = "logit"))

margins::margins_summary(logit_1)

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)
footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Shotspotter Activiated is the indicator variable which maps to when a police district received ShotSpotter technology.
                  Border Activiated is an indicator which maps to the earliest date in which a district has a bordering district that
                  has the ShotSpotter technology.
                  Number of Gun Injuries refers to the counts of victims that received gun injuries.
                  Probability of Gun Injury is a binary indicator equal to 1 if there was an instance of a gun-related injury.
                  Controls include officer hours, number of gun crimes, and number of other crimes.
                  "), ~str_remove_all(., "\n")) 



mean_dependent_panela <- map(list("(1)" = ols_v1, "(2)" = ols_v2, "(3)" = poisson_v1, "(4)" = poisson_v2),
    ~sprintf(as.double(fixest::fitstat(., type = "my")),fmt =  "%.3f")) %>% unlist()

mean_dependent_panela
mean_dependent_panelb <- map(list("(1)" = ols_prob_1, "(2)" = ols_prob_2, "(5)" = logit_1, "(6)" = logit_2),
                             ~sprintf(as.double(fixest::fitstat(., type = "my")),fmt =  "%.3f")) %>% unlist()





panelsummary_raw(list("(1)" = ols_v1, "(2)" = ols_v2, "(3)" = poisson_v1, "(4)" = poisson_v2),
                 list("(1)" = ols_prob_1, "(2)" = ols_prob_2, "(5)" = logit_1, "(6)" = logit_2),
                 stars = "econ",
                 coef_map = c( "treatment" = "ShotSpotter Activated",
                               "shotspot_border_treatment" = "Border Activated"),
                 gof_omit = "^R|A|B|S",
                 gof_map = gof_mapping) %>% 
  add_row(term = "Mean of Dependent Variable",
          `(1)` = mean_dependent_panela[[1]],
          `(2)` = mean_dependent_panela[[2]],
          `(3)` = mean_dependent_panela[[3]],
          `(4)` = mean_dependent_panela[[4]],
          `(5)` = "",
          `(6)` = "",
    .before = 5) %>% 
  add_row(term = "Mean of Dependent Variable",
          `(1)` = mean_dependent_panelb[[1]],
          `(2)` = mean_dependent_panelb[[2]],
          `(5)` = mean_dependent_panelb[[3]],
          `(6)` = mean_dependent_panelb[[4]],
          `(3)` = "",
          `(4)` = "",
          .before = 14) %>% 
  clean_raw(caption = "\\label{}Effect of ShotSpotter Activation on Gun Injury Victimization") %>% 
  pack_rows("Panel A: Number of Gun Injuries", 1, 8,
            italic = T,
            bold = F) %>% 
  pack_rows("Panel B: Probability of Gun Injury", 9, 16,
            italic = T,
            bold = F) %>% 
  add_header_above(c(" " = 1,
                     "OLS" = 2,
                     "Poisson" = 2,
                     "Logit" = 2)) %>% 
  add_header_above(c(" " = 1, "Estimation Method" = 6)) %>% 
  footnote(footnotes, threeparttable = T) 
  
  
