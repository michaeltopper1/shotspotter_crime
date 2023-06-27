library(tidyverse)
library(fixest)
library(did2s)
library(modelsummary)
library(kableExtra)
library(panelsummary)


victim_panel <- read_csv(here::here("analysis_data/xxvictim_panel.csv"))
if (!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}

dispatch_small <- dispatch_panel %>% 
  select(district, date,
    number_dispatches_1,number_dispatches_0, number_dispatches_2, number_dispatches_3)

victim_panel <- victim_panel %>% 
  inner_join(dispatch_small)

setFixest_fml(..ctrl = ~officer_hours +
                number_dispatches_1 + number_dispatches_2 + 
                number_dispatches_3 + number_dispatches_0| district + date)
dependent_variables <- c(
  "num_any_gunshot_victim",
  "num_gunshot_homicide",
  "num_non_gun_homicide",
  "num_any_homicide",
  "num_gun_robbery",
  "num_gun_battery")

vic_1 <- victim_panel %>% 
  feols(num_non_gun_homicide ~treatment + ..ctrl)

vic_2 <- victim_panel %>% 
  feols(num_any_gunshot_victim ~treatment + ..ctrl)

vic_3 <- victim_panel %>% 
  feols(num_gunshot_homicide ~treatment + ..ctrl)

vic_4 <- victim_panel %>% 
  feols(num_gun_robbery ~treatment + ..ctrl)

vic_5 <- victim_panel %>% 
  feols(num_gun_battery ~treatment + ..ctrl)

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01"
), ~str_remove_all(., "\n"))

f <- function(x) formatC(x, digits = 2, big.mark = ",", format = "f")

victim_table <- panelsummary(list(vic_1, vic_2, vic_3,
                  vic_4, vic_5),
             stars = "econ",
             mean_dependent = T,
             coef_map = c("treatment" = "ShotSpotter Activated",
                          "shotspot_border_treatment" = "Border District Activated"),
             gof_omit = "^R|A|B|S",
             gof_map = gof_mapping) %>% 
  add_header_above(c(
    " " = 1,
    "Homicide" = 1,
    "All" = 1,
    "Homicide" = 1,
    "Robbery" = 1,
    "Battery" = 1
    # "Theft" = 1,
  )) %>%
  add_header_above(c(
    " " = 1,
    "Non-Gun Related" = 1,
    "Gun-Related Victimization" =4
  )) %>%
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria")

