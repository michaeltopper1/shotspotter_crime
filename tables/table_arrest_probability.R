
library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)
library(ivDiag)

## WARNING FOR THIS SCRIPT:
## run reach of the AR tests by itself: do not run the entire script (eg Source)
## for some reason, the parallelization hangs and never completes if Sourcing

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



# creating percentages ----------------------------------------------------

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(arrest_made_p = arrest_made * 100,
         misc_letter_p_p = misc_letter_P *100,
         misc_letter_b_p = misc_letter_B * 100,
         misc_letter_f_p = misc_letter_F * 100)



# reduced form ------------------------------------------------------------

arrest_rate <- dispatch_panel_p1 %>% 
  feols(arrest_made*100 ~ treatment + ..ctrl, data = .)

misc_p <- dispatch_panel_p1 %>%
  feols(misc_letter_P*100 ~ treatment + ..ctrl)

misc_b <-  dispatch_panel_p1 %>%
  feols(misc_letter_B*100 ~ treatment + ..ctrl)

misc_f <-  dispatch_panel_p1 %>%
  feols(misc_letter_F*100 ~ treatment + ..ctrl)



# combining regressions ---------------------------------------------------

reduced_form <- list(arrest_rate, misc_p, misc_b, misc_f)


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: final_dispatch_description", "FE: Call-Type", 3,
                       "FE: hour", "FE: Hour-of-Day", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. All
                      coefficient estimates and means are in percentages.
                        The dependent variable in Column 1 is an indicator equal to one if a 911 call resulted in an arrest.
                      The dependent variable in Columns 2-4 is an indicator equal to one if a 911 call resulted in 
                      Other Police Service (Column 2), No Person Found (Column 3), or Peace Restored (Column 4).
                  Columns 2-4 report the three most frequent 911 final dispositions: Other Police Service, No Person Found, 
                  and Peace Restored. The final disposition is the final result of
                  what happened on the 911 call. Wild cluster bootstrap p-values using 999 replications are also reported
                  since the number of clusters (22) is below the threshold of 30 put forth in
                  Cameron et al. (2008).
                  "), ~str_remove_all(., "\n"))


## gathered from fwildbootstrap file: wildclusterboot_arrest.R
## too difficult to implement directly in this script. Reason for hard-code.
wild_bootstrap_arrest <- c('0.002', '0.039', '0.002', '0.001')


arrest_table_raw <- panelsummary_raw(reduced_form, mean_dependent = T,
                                     stars = 'econ', gof_map = gof_mapping,
                                     coef_map = c(
                                       "treatment" = "ShotSpotter Activated",
                                       "fit_entry_to_dispatch" = "Call-to-Dispatch"
                                     ),
                                     gof_omit = "^R|A|B|S")

arrest_prob <- arrest_table_raw %>% 
  slice(-c(5:8)) %>% 
  janitor::clean_names() %>% 
  add_row(term = "Wild Bootstrap P-Value",model_1 = "0.002", model_2 = "0.039",
          model_3 = "0.002", model_4 = "0.001", .before = 5) %>% 
  clean_raw(pretty_num = T,
            caption = "\\label{arrest_prob}Effect of ShotSpotter on 911 Call Resolutions (OLS)",
            format = "latex") %>% 
  add_header_above(c(" " = 1,
                     "Arrest\nMade" = 1,
                     "Other\nPolice Service" = 1,
                     "No\nPerson Found" =1,
                     "Peace\nRestored" = 1)) %>% 
  add_header_above(c(" " = 2,
                     "Most Frequent Final 911 Dispositions" = 3)) %>% 
  row_spec(5, hline_after = TRUE) %>%
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(arrest_prob, "paper/tables/arrest_prob.tex")



# making appendix table here with first stage -----------------------------


