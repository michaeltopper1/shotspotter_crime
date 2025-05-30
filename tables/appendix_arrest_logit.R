
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
  feglm(arrest_made ~ treatment + ..ctrl, data = .,
        family = "logit")


misc_p <- dispatch_panel_p1 %>%
  feglm(misc_letter_P ~ treatment + ..ctrl,
        family = "logit")

misc_b <-  dispatch_panel_p1 %>%
  feglm(misc_letter_B ~ treatment + ..ctrl,
        family = "logit")

misc_f <-  dispatch_panel_p1 %>%
  feglm(misc_letter_F ~ treatment + ..ctrl, 
        family = "logit")



gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district.
                      The dependent variable in Column 1 is an indicator equal to one if a 911 call resulted in an arrest.
                      The dependent variable in Columns 2-4 is an indicator equal to one if a 911 call resulted in 
                      Other Police Service (Column 2), No Person Found (Column 3), or Peace Restored (Column 4).
                  Columns 2-4 report the three most frequent 911 final dispositions: Other Police Service, No Person Found, 
                  and Peace Restored. The final disposition is the final result of
                  what happened on the 911 call.
                  "), ~str_remove_all(., "\n"))

arrest_table_raw <-
  panelsummary_raw(
    list(
      arrest_rate,
      misc_p,
      misc_b,
      misc_f
    ),
    mean_dependent = T,
    stars = "econ",
    coef_map = c(
      "treatment" = "ShotSpotter Activated",
      "shotspot_border_treatment" = "Border Activated"
    ),
    gof_omit = "^R|A|B|S",
    fmt = 3,
    gof_map = gof_mapping
  ) 


arrest_prob_logit <- arrest_table_raw %>% 
  janitor::clean_names() %>% 
  clean_raw(pretty_num = T,
            caption = "\\label{arrest_prob_logit}Effect of ShotSpotter on 911 Call Resolutions (Logit)",
            format = "latex") %>% 
  row_spec(4, hline_after = T) %>% 
  add_header_above(c(" " = 1,
                     "Arrest\nMade" = 1,
                     "Other\nPolice Service" = 1,
                     "No\nPerson Found" =1,
                     "Peace\nRestored" = 1)) %>% 
  add_header_above(c(" " = 2,
                     "Most Frequent Final 911 Dispositions" = 3)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(arrest_prob_logit, "paper/appendix_tables/appendix_arrest_logit.tex")
