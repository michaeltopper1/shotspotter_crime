
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



# 2sls --------------------------------------------------------------------

arrest_made_iv <- dispatch_panel_p1 %>% 
  feols(arrest_made*100 ~ 1 | district + date + final_dispatch_description +
          hour| entry_to_dispatch ~ treatment,
        cluster = "district")

misc_p_iv <- dispatch_panel_p1 %>% 
  feols(misc_letter_P*100 ~ 1 | district + date + final_dispatch_description +
          hour| entry_to_dispatch ~ treatment,
        cluster = "district")


misc_b_iv <- dispatch_panel_p1 %>% 
  feols(misc_letter_B*100 ~ 1 | district + date + final_dispatch_description +
          hour| entry_to_dispatch ~ treatment,
        cluster = "district")

misc_f_iv <- dispatch_panel_p1 %>% 
  feols(misc_letter_F*100 ~ 1 | district + date + final_dispatch_description +
          hour| entry_to_dispatch ~ treatment,
        cluster = "district")



# extracting test statistics ----------------------------------------------

# getting the eff F -------------------------------------------------------


# f_eff <- eff_F(dispatch_panel_p1,
#       Y = "arrest_made",
#       D = "entry_to_dispatch",
#       Z = "treatment",
#       FE = c("final_dispatch_description", 
#              "hour",
#              "date",
#              "district"),
#       cl = "district")

## these are equivalent in the case of one instrumnet
f_eff <- fitstat(arrest_made_iv, 'kpr')

kpr <- f_eff$kpr$stat[1,1] %>% sprintf("%.3f",.)



# correcting for weak instrument inference --------------------------------


## run reach of these by itself: do not run the entire script
## for some reason, the parallelization hangs and never completes if Sourcing

ar_1 <- AR_test(dispatch_panel_p1, 
                Y = "arrest_made_p", 
                D = "entry_to_dispatch", 
                Z = "treatment",
                controls = NULL, 
                FE = c("final_dispatch_description", 
                       "hour",
                       "date",
                       "district"), 
                cl = "district", 
                weights = NULL, 
                prec = 3, CI = TRUE, alpha = 0.05, parallel = NULL, cores = NULL)

ar_2 <- AR_test(dispatch_panel_p1, 
                Y = "misc_letter_p_p", 
                D = "entry_to_dispatch", 
                Z = "treatment",
                controls = NULL, 
                FE = c("final_dispatch_description", 
                       "hour",
                       "date",
                       "district"), 
                cl = "district", 
                weights = NULL, 
                prec = 3, CI = TRUE, alpha = 0.05, parallel = NULL, cores = NULL)

ar_3 <- AR_test(dispatch_panel_p1, 
                Y = "misc_letter_b_p", 
                D = "entry_to_dispatch", 
                Z = "treatment",
                controls = NULL, 
                FE = c("final_dispatch_description", 
                       "hour",
                       "date",
                       "district"), 
                cl = "district", 
                weights = NULL, 
                prec = 3, CI = TRUE, alpha = 0.05, parallel = NULL, cores = NULL)

ar_4 <- AR_test(dispatch_panel_p1, 
                Y = "misc_letter_f_p", 
                D = "entry_to_dispatch", 
                Z = "treatment",
                controls = NULL, 
                FE = c("final_dispatch_description", 
                       "hour",
                       "date",
                       "district"), 
                cl = "district", 
                weights = NULL, 
                prec = 3, CI = TRUE, alpha = 0.05, parallel = NULL, cores = NULL)


# confidence intervals ----------------------------------------------------

ar_1_conf <- ar_1 %>% pluck(2)

ar_2_conf <- ar_2 %>% pluck(2)

ar_3_conf <- ar_3 %>% pluck(2)

ar_4_conf <- ar_4 %>% pluck(2)


# AR pvalue ---------------------------------------------------------------

ar_1_pvalue <- ar_1$Fstat[4] %>% sprintf("%.3f",.)

ar_2_pvalue <- ar_2$Fstat[4] %>% sprintf("%.3f",.)

ar_3_pvalue <- ar_3$Fstat[4] %>% sprintf("%.3f",.)

ar_4_pvalue <- ar_4$Fstat[4] %>% sprintf("%.3f",.)


# combining regressions ---------------------------------------------------

reduced_form <- list(arrest_rate, misc_p, misc_b, misc_f)

iv_regs <- list(arrest_made_iv, misc_p_iv, misc_b_iv, misc_f_iv)

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
                  what happened on the 911 call. 
                  Panel A shows the reduced form estimates of the ShotSpotter implementation, while 
                      Panel B shows the second stage of a 2SLS regression where Call-to-Dispatch time is the
                      endogenous variable, and ShotSpotter implementation is the instrument.
                  Hence, Panel B estimates show the marginal effect of an extra second of Call-to-Dispatch on
                  arrest probability for calls that are induced to have longer Call-to-Dispatch times by ShotSpotter (compliers).
                  First stage estimates are shown in the main results table where ShotSpotter implementation results in ~60
                  second increase for Call-to-Dispatch times on average.
                  In Panel A, Wild cluster bootstrap p-values using 999 replications are also reported
                  since the number of clusters (22) is below the threshold of 30 put forth in
                  Cameron et al. (2008).
                  In Panel B, we report the effective F-statistic (Eff F-Stat) from Olea and Pflueger (2013), and the confidence intervals and corresponding
                  p-values from the Anderson and Rubin (1949) test which is robust to weak instruments.
                  "), ~str_remove_all(., "\n"))

wild_bootstrap_arrest <- c('0.002', '0.039', '0.002', '0.001')


arrest_table_raw <- panelsummary_raw(reduced_form, iv_regs, mean_dependent = T,
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
  add_row(term = "Eff F-Stat",model_1 = kpr, 
          model_2 = kpr, model_3 = kpr, model_4 = kpr, .before = 10) %>% 
  add_row(term = "AR Conf.Int",model_1 = ar_1_conf, 
          model_2 = ar_2_conf, model_3 = ar_3_conf, model_4 = ar_4_conf, .before = 11) %>% 
  add_row(term = "AR P-Value",model_1 = ar_1_pvalue, 
          model_2 = ar_2_pvalue, model_3 = ar_3_pvalue, model_4 = ar_4_pvalue, .before = 12) %>% 
  clean_raw(pretty_num = T,
            caption = "\\label{arrest_prob}Effect of ShotSpotter on 911 Call Resolutions (OLS)",
            format = "latex") %>% 
  pack_rows(group_label = "Panel A: Reduced Form", 1, 5, italic = T, bold = F) %>% 
  pack_rows(group_label = 'Panel B: 2SLS (Second Stage)', 6, 12, hline_after = F,
             latex_gap_space = "0.5cm", italic = T, bold = F) %>% 
  add_header_above(c(" " = 1,
                     "Arrest\nMade" = 1,
                     "Other\nPolice Service" = 1,
                     "No\nPerson Found" =1,
                     "Peace\nRestored" = 1)) %>% 
  add_header_above(c(" " = 2,
                     "Most Frequent Final 911 Dispositions" = 3)) %>% 
  row_spec(12, hline_after = TRUE) %>%
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(arrest_prob, "paper/tables/arrest_prob.tex")



# making appendix table here with first stage -----------------------------


