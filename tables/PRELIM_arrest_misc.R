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

## need to add in other arrest mades. This doesn't change the results as far as I have looked.
## 
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  tidylog::mutate(arrest_made = if_else(str_detect(final_disposition_code, "R") | arrest_made == 1, 1, 0)) %>% 
  mutate(arrest_made = replace_na(arrest_made, 0)) 



# misc codes or not -------------------------------------------------------


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(misc_code = if_else(str_detect(final_disposition_code, "^\\d{1,2}[A-Z]{1,2}"), 1, 0) %>% 
           replace_na(0))

dispatch_panel_p1 %>% 
  feols(misc_code ~ treatment | date + district + hour + final_dispatch_description,
        cluster = ~district) 

did2s(data = dispatch_panel_p1,
      yname = "misc_code",
      first_stage = ~0|district + date + 
        final_dispatch_code + hour,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district") 


# within misc codes -------------------------------------------------------

## misc codes top 20 account for 87% 
dispatch_panel_p1 %>% 
  filter(misc_code == 1) %>% 
  count(final_disposition_code, sort = T) %>% 
  mutate(total = sum(n),
         fraction = n/total) %>% 
  head(20) %>% 
  mutate(sum(fraction))

top_20_misc_codes <- dispatch_panel_p1 %>% 
  filter(misc_code == 1) %>% 
  count(final_disposition_code, sort = T) %>% 
  head(20) %>% 
  pull(final_disposition_code)

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(misc_codes = if_else(final_disposition_code %in% top_20_misc_codes, final_disposition_code,
                                                 "Other"))

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  fastDummies::dummy_cols("misc_codes") 

y_variables <- dispatch_panel_p1 %>% colnames() %>% 
  as_tibble() %>% 
  filter(str_detect(value,"^misc_codes_")) %>% pull()




misc_code_regressions <- dispatch_panel_p1 %>% 
  feols(c(misc_codes_1B ,   misc_codes_1E ,   misc_codes_1F ,   misc_codes_1O,  misc_codes_1P,   
          misc_codes_5B ,   misc_codes_5E ,   misc_codes_5F,    misc_codes_5P ,   misc_codes_11B , 
          misc_codes_11P ,  misc_codes_14P ,  misc_codes_15I,   misc_codes_15P ,  misc_codes_16P,  
          misc_codes_19A ,  misc_codes_19B  , misc_codes_19E , misc_codes_19P ,  misc_codes_19PZ ,
          misc_codes_Other)~treatment | date + district + hour + final_dispatch_description,
        cluster = ~district)

misc_code_regressions %>% 
  coeftable() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(pr_t <0.05)



misc_letters <- dispatch_panel_p1 %>% 
  filter(misc_code == 1) %>% 
  mutate(misc_letter = str_extract(final_disposition_code, "[A-Z]{1,2}$")) %>% 
  count(misc_letter, sort = T) %>% 
  head(20) %>% pull(misc_letter)

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(letter = str_extract(final_disposition_code, "[A-Z]{1,2}$")) %>% 
  mutate(misc_letter = if_else(letter %in% misc_letters, letter, "Other")) %>% 
  fastDummies::dummy_cols("misc_letter") 


misc_letters_regressions <- dispatch_panel_p1 %>% 
  feols(c(
    misc_letter_A, 
    misc_letter_B, 
    misc_letter_BZ, 
    misc_letter_C, 
    misc_letter_D, 
    misc_letter_E, 
    misc_letter_EZ, 
    misc_letter_F, 
    misc_letter_FZ, 
    misc_letter_H, 
    misc_letter_I, 
    misc_letter_IZ, 
    misc_letter_J, 
    misc_letter_K, 
    misc_letter_N, 
    misc_letter_O, 
    misc_letter_Other, 
    misc_letter_P, 
    misc_letter_PZ, 
    misc_letter_X, 
    misc_letter_Z
  ) ~ treatment| date + district + hour + final_dispatch_description,
  cluster = ~district) %>% coeftable()

misc_letters_regressions %>% 
  janitor::clean_names() %>% 
  filter(pr_t < 0.05)

dispatch_panel_p1 %>% 
  count(letter, sort = T)
