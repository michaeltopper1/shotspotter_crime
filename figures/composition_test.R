
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

# Set up future plan for parallel processing
plan(multisession, workers =6)
options(future.globals.maxSize = 8 * 1024^3)  # Set to 8 GB


# Combine both operations into a single future_map
results_combined <- future_map(columns_to_estimate, function(column) {
  # Construct the formula
  formula <- as.formula(paste(column, "~ treatment | district + date + hour"))
  
  ## run and save the regression
  regression <- feols(formula, cluster = "district", data = dispatch_panel_p1)
  regression_tidy <- regression %>% 
    broom::tidy(conf.int = T)
  
  # Run the regression and extract the p-value
  pvalue <- regression %>% 
    pvalue()

  # get the coefficients
  coefs <- coef(regression)["treatment"]
  
  # get the dependent variable means
  mean_y <- regression %>% fitstat('my') %>% unlist() %>% as.double()
  
  ## get the confidence intervals
  conf_low <- regression_tidy %>% 
    filter(term == "treatment") %>% 
    pull(conf.low)
  
  conf_high <- regression_tidy %>% 
    filter(term == "treatment") %>% 
    pull(conf.high)
  
  # Count instances in data where column == 1
  count <- dispatch_panel_p1 %>% 
    count(!!sym(column)) %>% 
    filter(!!sym(column) == 1) %>% pull(n)
  
  # Return the p-value, column name, and count as a named list
  list(pvalue = pvalue, column = column, count = count, coefs = coefs,
       conf_low = conf_low, conf_high = conf_high,
       mean_y = mean_y )
})

# Extracting the results into separate vectors/tibbles
pvalues <- map_dbl(results_combined, "pvalue")
codes <- map_chr(results_combined, "column")
instances_in_data <- map_dbl(results_combined, "count")
estimates <- map_dbl(results_combined, "coefs")
conf_low <- map_dbl(results_combined, 'conf_low')
conf_high <- map_dbl(results_combined, 'conf_high')
mean_y <- map_dbl(results_combined, 'mean_y')

# converting to tibbles ---------------------------------------------------

instances_in_data <- as_tibble(instances_in_data) %>% 
  rename(n = value)

results <- as_tibble(pvalues) %>% 
  rename(pvalue = value)

result_column <- as_tibble(codes) %>% 
  rename(column = value)

estimates_columns <- as_tibble(estimates) %>% 
  rename(estimate = value)

conf_low <- as_tibble(conf_low) %>% 
  rename(conf_low = value)

conf_high <- as_tibble(conf_high) %>% 
  rename(conf_high = value)

mean_y <- as_tibble(mean_y) %>% 
  rename(mean_y = value)


# binding -----------------------------------------------------------------

results <- results %>%
  bind_cols(result_column) %>%
  bind_cols(instances_in_data) %>% 
  bind_cols(estimates_columns) %>% 
  bind_cols(conf_low, conf_high, mean_y)

# Close parallel workers
plan(sequential)


## correcting for multiple hypothesis

results <- results %>% 
  mutate(pvalue_fdr = p.adjust(pvalue, "fdr")) 

pvalue_hist <- results %>%
  ggplot(aes(pvalue_fdr)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "dark red") +
  labs(x = "FDR-Corrected P-value",
       y = "Number of Call-types")


ggsave(pvalue_hist, filename = "presentations/figures/pvalue_hist.jpeg", width = 7, height = 5)
