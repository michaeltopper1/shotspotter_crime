## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2024-10-03
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)
library(furrr)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}

theme_set(theme_minimal())

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(call_type = final_dispatch_code) %>% 
  fastDummies::dummy_cols("call_type")

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  janitor::clean_names()

columns_to_estimate <- dispatch_panel_p1 %>% 
  janitor::clean_names() %>% 
  select(matches("type_")) %>% 
  colnames()


pvalues <- rep(0, 189)
codes <- rep("", 189)
for (i in 1:length(columns_to_estimate)){
  print(i)
  # Construct the formula
  formula <- as.formula(paste(columns_to_estimate[i], "~ treatment | district + date + hour"))
  
  # Run the regression
  reg_value <- feols(formula, cluster = "district", data = dispatch_panel_p1) %>% 
    pvalue()
  pvalues[i] <- reg_value
  codes[i] <- columns_to_estimate[i]
}

instances_in_data <- rep(0, 189)
for (i in 1:length(columns_to_estimate)){
  print(i)
  count <- dispatch_panel_p1 %>% 
    count(!!sym(columns_to_estimate[i])) %>% 
    filter(!!sym(columns_to_estimate[i]) == 1) %>% pull(n)
  
  instances_in_data[i] <- count
}



# converting to tibbles ---------------------------------------------------

instances_in_data <- instances_in_data %>% 
  as_tibble() %>% 
  rename(n = value)


results <- pvalues %>% 
  as_tibble() %>% 
  rename(pvalue = value)

result_column <- codes %>% 
  as_tibble() %>% 
  rename(column = value)


# binding -----------------------------------------------------------------


results <- results %>% 
  bind_cols(result_column)

results <- results %>% 
  bind_cols(instances_in_data)

## correcting for multiple hypothesis tests
results <- results %>% 
  mutate(pvalue_fdr = p.adjust(pvalue, "fdr")) 

results %>% 
  arrange(pvalue_fdr)

dispatch_panel_p1 %>% 
  feols(call_type_shotsf ~ treatment | district + date + hour)


# how do these affect response times? -------------------------------------
dispatch_panel_p1 %>% 
  filter(call_type_bankhu == 1) %>% 
  feols(c(entry_to_dispatch, entry_to_onscene) ~ treatment | district + date + hour)
dispatch_panel_p1 %>% 
  filter(call_type_batip == 1) %>% 
  feols(c(entry_to_dispatch, entry_to_onscene) ~ treatment | district + date + hour)
dispatch_panel_p1 %>% 
  feols(call_type_dui ~ treatment | district + date + hour)
results %>%
  ggplot(aes(pvalue_fdr)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "dark red")

results %>% 
  ggplot(aes(pvalue_fdr)) +
  geom_density() +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "dark red")

 