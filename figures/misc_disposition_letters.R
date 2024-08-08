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

# misc codes or not -------------------------------------------------------

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(misc_code = if_else(str_detect(final_disposition_code, "^\\d{1,2}[A-Z]{1,2}"), 1, 0) %>% 
           replace_na(0))


# getting all dispositions that end with letters --------------------------

misc_letters <- dispatch_panel_p1 %>% 
  filter(misc_code == 1) %>% 
  mutate(misc_letter = str_extract(final_disposition_code, "[A-Z]{1,2}$")) %>% 
  count(misc_letter, sort = T) %>% 
  head(20) %>% pull(misc_letter)


## creating dummy columns for each ending letter combination
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(letter = str_extract(final_disposition_code, "[A-Z]{1,2}$")) %>% 
  mutate(misc_letter = if_else(letter %in% misc_letters, letter, "Other")) %>% 
  fastDummies::dummy_cols("misc_letter") 

## getting the fraction within sample for each letter combination. This will help with
## the glue:: component of the graph to show which dispositions are most frequent or not
misc_letter_fractions <- dispatch_panel_p1 %>% 
  summarize(across(starts_with("misc_letter_"), ~sum(.,na.rm = T)/n())) %>% 
  pivot_longer(cols = everything(),names_to = "letter", values_to = "fraction")

## this is to get all of these in percentages in the regressions
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(across(starts_with("misc_letter_"), ~.*100))

## all regressions
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


## joining the fraction and the regressions
misc_letters_regressions <- misc_letters_regressions %>% 
  left_join(misc_letter_fractions, join_by(lhs == letter))

## renaming for the graph
misc_letters_regressions <- misc_letters_regressions %>% 
  mutate(disposition = case_when(
    lhs == "misc_letter_A" ~ "Not Bona Fide",
    lhs == "misc_letter_B" ~ "No Person Found",
    lhs == "misc_letter_BZ" ~ "Mental Health: No Person Found",
    lhs == "misc_letter_C" ~ "No Such Address",
    lhs == "misc_letter_D" ~ "No Police Necessary",
    lhs == "misc_letter_E" ~ "Perpetrator Gone on Arrival",
    lhs == "misc_letter_EZ" ~ "Mental Health: Perpetrator Gone on Arrival",
    lhs == "misc_letter_F" ~ "Peace Restored",
    lhs == "misc_letter_FZ" ~ "Mental Health: Peace Restored",
    lhs == "misc_letter_H" ~ "Recontact Police if Return",
    lhs == "misc_letter_I" ~ "Removed to Hospital/Detox.",
    lhs == "misc_letter_IZ" ~ "Mental Health: Removed to Hospital/Detox.",
    lhs == "misc_letter_J" ~ "Returned to Family or HOme",
    lhs == "misc_letter_K" ~ "Taken to District Station",
    lhs == "misc_letter_N" ~ "Ordinance Complaint Issued",
    lhs == "misc_letter_O" ~ "Advised Legal Help",
    lhs == "misc_letter_P" ~ "Other Police Service",
    lhs == "misc_letter_PZ" ~ "Mental Health: Other Police Service",
    lhs == "misc_letter_X" ~ "Missle X Completed",
    lhs == "misc_letter_Z" ~ "Mental Health",
    lhs == "misc_letter_Other" ~ "All Other Dispositions"
  ))

## easier to read
misc_letters_regressions <- misc_letters_regressions %>% as_tibble() %>% 
  janitor::clean_names()


## final graph 
misc_letters_graph <- misc_letters_regressions %>% 
  mutate(fraction = fraction *100) %>% 
  mutate(fraction_char = sprintf("%.3f", fraction), .before = 1) %>% 
  mutate(disposition = glue::glue("{disposition}\n({fraction_char}%)")) %>% 
  mutate(disposition = fct_reorder(disposition,fraction)) %>% 
  mutate(conf_left = estimate - std_error*1.96,
         conf_right = estimate + std_error *1.96) %>% 
  ggplot(aes(estimate, disposition)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf_left, xmax = conf_right),
                width = 0.3) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "dark red") +
  labs(x = "Point Estimate (percentage points) and 95% Confidence Interval",
       y = "Final Disposition Description") +
  theme_minimal()

ggsave(misc_letters_graph, filename = "paper/appendix_figures/misc_disposition_letters_.jpeg", width = 7, height = 5)

