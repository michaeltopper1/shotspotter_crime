
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



dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district)

## for wild cluster boot only
# dispatch_panel_p1 <- dispatch_panel_p1 %>% 
#   mutate(date = as.integer(date),
#          final_dispatch_code = match(final_dispatch_code, unique(final_dispatch_code)))

# entry to dispatch -------------------------------------------------------

entry_d <- feols(entry_to_dispatch ~ treatment | district + date,
                 cluster = ~district,
                 data = dispatch_panel_p1)

entry_d_1 <- feols(entry_to_dispatch ~ treatment | district + date +
                     hour + final_dispatch_description,
                   cluster = ~district,
                   data = dispatch_panel_p1)

entry_d_2 <- feols(entry_to_dispatch ~ treatment  +
                     officer_hours + number_dispatches| district + date +
                     final_dispatch_description + hour,
                   cluster = ~district,
                   data = dispatch_panel_p1)

entry_d_3 <- did2s(data = dispatch_panel_p1,
                   yname = "entry_to_dispatch",
                   first_stage = ~0|district + date + 
                     final_dispatch_description + hour,
                   second_stage = ~treatment,
                   treatment = "treatment",
                   cluster_var = "district")

entry_d_4 <- feols(entry_to_dispatch ~ treatment + shotspot_border_treatment| district + date +
                     final_dispatch_description + hour,
                   cluster = ~district,
                   data = dispatch_panel_p1)



footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01"
), ~str_remove_all(., "\n"))

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

dispatch_raw <- panelsummary_raw(list(entry_d, entry_d_1,
                      entry_d_3, entry_d_4),
                 stars = "econ",
                 mean_dependent = T,
                 coef_map = c("treatment" = "ShotSpotter Activated",
                              "shotspot_border_treatment" = "Border District Activated"),
                 gof_omit = "^R|A|B|S",
                 gof_map = gof_mapping) %>% 
  janitor::clean_names()

dispatch_table <- dispatch_raw %>% 
  select(-model_1) %>% 
  mutate(model_3 = if_else(term == "FE: District" |
                             term == "FE: Day-by-Month-by-Year", "X", model_3)) %>%
  mutate(model_3 = if_else(term == "FE: Call-Type" |
                             term == "FE: Hour-of-Day", "X", model_3)) %>% 
  mutate(model_3 = if_else(term == "Mean of Dependent Variable", 
                           model_2, model_3)) %>% 
  add_row(term = "Wild Bootstrap P-Value", model_2 = "0.012",
          model_3 = "", model_4 = "0.017", .before = 7) %>% 
  add_row(term = "Gardner (2022) Robust", model_2 = "No", model_3 = "Yes", model_4 = "No") %>% 
  slice(-c(8:11)) %>% 
  clean_raw(pretty_num = T,
            format = "html",
            caption = "Effect of ShotSpotter Enactment on Call-to-Dispatch (OLS)") %>% 
  row_spec(7, hline_after = T) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

write_file(dispatch_table, file = "presentations/tables/dispatch_table.html")

