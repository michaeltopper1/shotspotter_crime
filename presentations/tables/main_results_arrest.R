
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

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)




# new ---------------------------------------------------------------------

# getting disposition letters ---------------------------------------------

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(misc_code = if_else(str_detect(final_disposition_code, "^\\d{1,2}[A-Z]{1,2}"), 1, 0) %>% 
           replace_na(0))

misc_letters <- dispatch_panel_p1 %>% 
  filter(misc_code == 1) %>% 
  mutate(misc_letter = str_extract(final_disposition_code, "[A-Z]{1,2}$")) %>% 
  count(misc_letter, sort = T) %>% 
  head(20) %>% pull(misc_letter)

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(letter = str_extract(final_disposition_code, "[A-Z]{1,2}$")) %>% 
  mutate(misc_letter = if_else(letter %in% misc_letters, letter, "Other")) %>% 
  fastDummies::dummy_cols("misc_letter") 

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(all_other_disp = if_else(misc_letter_B != 1 & misc_letter_P != 1 & misc_letter_F != 1, 1 ,0)) 


arrest_rate <- dispatch_panel_p1 %>% 
  feols(arrest_made*100 ~ treatment + ..ctrl, data = .)

arrest_rate_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 1) %>% 
  feols(arrest_made*100 ~ treatment + ..ctrl, data = .)

arrest_rate_no_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report != 1) %>% 
  feols(arrest_made*100 ~ treatment + ..ctrl, data = .)

misc_p <- dispatch_panel_p1 %>%
  feols(misc_letter_P*100 ~ treatment + ..ctrl)

misc_b <-  dispatch_panel_p1 %>%
  feols(misc_letter_B*100 ~ treatment + ..ctrl)

misc_f <-  dispatch_panel_p1 %>%
  feols(misc_letter_F*100 ~ treatment + ..ctrl)




# old ---------------------------------------------------------------------


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

arrest_table_raw <- panelsummary_raw(list(
  arrest_rate,
  arrest_rate_gun,
  arrest_rate_no_gun,
  misc_p,
  misc_b,
  misc_f
),
mean_dependent = T, stars = "econ",
coef_map = c( "treatment" = "ShotSpotter Activated",
              "shotspot_border_treatment" = "Border Activated"),
gof_omit = "^R|A|B|S",
fmt = 3,
gof_map = gof_mapping) %>% 
  slice(-c(5:8))

wild_bootstrap_arrest <- c('0.001', '0.311', '0.005', '0.122', '0.006', '0.005')

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors clustered by district. 
                      "
), ~str_remove_all(., "\n"))

arrest_table <- arrest_table_raw %>% 
  janitor::clean_names() %>% 
  add_row(term = "Wild Bootstrap P-Value",model_1 = "0.001", model_2 = "0.311",
          model_3 = "0.005", model_4 = "0.122",
          model_5 = "0.006", model_6 = "0.005", .before = 5) %>%
  add_row(term = "Clusters",model_1 = "22", model_2 = "22",
          model_3 = "22", model_4 = "22",
          model_5 = "22", model_6 = "22", .before = 6) %>% 
  clean_raw(pretty_num = T, format = 'html') %>% 
  add_header_above(c(" " = 1,
                     "Pooled" = 1,
                     "Gun" = 1,
                     "Non-Gun" = 1,
                     "Other\nPolice Service" = 1,
                     "No\nPerson Found" =1,
                     "Peace\nRestored" = 1)) %>% 
  add_header_above(c(" " = 2,
                     "Gun-Relation" = 2,
                     "Most Frequent Final Dispositions" = 3)) %>% 
  add_header_above(c(" " = 1, "Effect on Arrest Likelihood (percentage)" = 6)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = T, html_font = "Cambria")

arrest_table_pooled_talk <- arrest_table %>% 
  column_spec(2, background = "yellow") 

arrest_table_gun_talk <- arrest_table %>% 
  column_spec(3, background = "yellow") %>% 
  column_spec(4, background = "pink")

arrest_table_type_talk <- arrest_table %>% 
  column_spec(5, background = "yellow") %>% 
  column_spec(6, background = "pink") %>% 
  column_spec(7, background = "lightblue")


write_file(arrest_table, file = "presentations/tables/arrest_table.html")
write_file(arrest_table_pooled_talk, file = "presentations/tables/arrest_table_pooled_talk.html")
write_file(arrest_table_gun_talk, file = "presentations/tables/arrest_table_gun_talk.html")
write_file(arrest_table_type_talk, file = "presentations/tables/arrest_table_type_talk.html")

