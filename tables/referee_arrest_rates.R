
library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)
library(patchwork)
library(modelsummary)
library(kableExtra)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
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

dispatch_panel_p1 %>% 
  filter(misc_code ==0  | arrest_made == 1) %>% 
  count(arrest_made) %>% 
  mutate(n/sum(n))

top_20 <- dispatch_panel_p1 %>% 
  count(final_dispatch_description, sort = T) %>% 
  head(20) %>% 
  pull(final_dispatch_description)



arrest_rate_all <- dispatch_panel_p1 %>% 
  filter(final_dispatch_description %in% top_20) %>%
  datasummary(arrest_made *final_dispatch_description ~ (Mean + N) , data = .,
              output = 'data.frame', fmt = 3) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(-x)

arrest_rate_nomisc <- dispatch_panel_p1 %>% 
  filter(final_dispatch_description %in% top_20) %>%
  filter(misc_code == 0) %>% 
  datasummary(arrest_made *final_dispatch_description ~ (Mean + N) , data = .,
              output = 'data.frame', fmt = 3) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(-x) %>% 
  rename( mean_misc = mean,
          n_misc = n)

arrest_rate_table_raw <- arrest_rate_all %>% 
  left_join(arrest_rate_nomisc)



total_arrest_made <- dispatch_panel_p1 %>% 
  summarize(mean = mean(arrest_made, na.rm = T),
            n = n()) %>% 
  mutate(final_dispatch_description ="All Calls")

total_arrest_made_misc <- dispatch_panel_p1 %>% 
  filter(misc_code == 0) %>% 
  summarize(mean_misc = mean(arrest_made, na.rm = T),
            n_misc = n()) %>% 
  mutate(final_dispatch_description ="All Calls")

total_arrest <- total_arrest_made %>% 
  left_join(total_arrest_made_misc) 

total_arrest <- total_arrest %>% 
  mutate(across(-c(n, n_misc, final_dispatch_description), ~sprintf("%.3f", .))) %>% 
  mutate(across(everything(), ~ as.character(.)))


footnote <- map(list( "Data is at
         the call-level. These 20 call-types represent approximately 88 percent of all Priority 1 calls.
         Any description
         ending with IP stands for `in progress', while any description ending in JO 
         stands for `just occurred.' The columns nested under Fraction Arrested shows the 
         fraction of calls that ended in an arrest. Excluding MISC are only calls 
         that did not end with a final disposition of `miscellaneous'. A final disposition of
         `miscellaneous' is reserved for the purpose of recording police action taken in minor non-criminal matters
or minor violations of city ordinances of the type not specifically provided for on case reports.
                  "), ~str_remove_all(., "\n"))


arrest_table <- arrest_rate_table_raw %>% 
  mutate(n = as.double(n)) %>% 
  arrange(desc(n)) %>% 
  mutate(n = as.character(n)) %>% 
  bind_rows(total_arrest) %>% 
  mutate(n = as.double(n)) %>% 
  mutate(fraction = n/nrow(dispatch_panel_p1)) %>% 
  select(-starts_with("n")) %>% 
  select(final_dispatch_description, fraction, mean, mean_misc) %>% 
  mutate(fraction = sprintf("%.3f", fraction)) %>% 
  mutate(final_dispatch_description = str_to_title(final_dispatch_description)) %>% 
  mutate(final_dispatch_description = str_replace(final_dispatch_description, "Jo", "JO"),
         final_dispatch_description = str_replace(final_dispatch_description, "Ip", "IP")) %>% 
  # mutate(across(-c(final_dispatch_description), ~ . %>% 
  #                 as.double() %>% scales::percent()))
  kbl(col.names = c("Call-Type", 
      "Fraction of Total Calls", "All Calls",
      "Excluding MISC"),
      booktabs = T,
      caption = "\\label{referee_arrest_rates}Arrest Rates for 20 Most Frequent Call-Types",
      format = "latex") %>% 
  kable_styling() %>% 
  add_header_above(c(' ' = 2,
                     'Fraction Arrested' = 2)) %>% 
  row_spec(row = 20, hline_after = T) %>% 
  row_spec(row = 21, bold = F, italic = T) %>% 
  footnote(footnote, threeparttable = T)

writeLines(arrest_table, "paper/appendix_tables/referee_arrest_rate.tex")

