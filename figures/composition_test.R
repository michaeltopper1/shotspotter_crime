
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

## getting codes for those calls that appear at least 1 time per year
## doing this so there is sufficient variation in estimation
once_a_year_codes <- dispatch_panel_p1 %>%
  count(final_dispatch_code,year, sort = T) %>%
  complete(final_dispatch_code, year = 2016:2022) %>%
  group_by(final_dispatch_code) %>%
  mutate(no_yearly_appearance = any(is.na(n))) %>%
  ungroup() %>%
  filter(no_yearly_appearance == F) %>%
  distinct(final_dispatch_code) %>% pull()

## creating dummy columns for estimation
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(call_type = if_else(final_dispatch_code %in% once_a_year_codes,
                             final_dispatch_code, 'bad_code')) %>%
  fastDummies::dummy_cols("call_type")

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  janitor::clean_names()

## getting the columns I want to estimate/loop through
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
  mutate(pvalue_fdr = p.adjust(pvalue, "fdr"),
         pvalue_bonferonni = p.adjust(pvalue, "bonferroni")) 

results <- results %>% 
  mutate(significant_pos = if_else(pvalue < 0.05 & estimate > 0, 1, 0),
         significant_pos_fdr = if_else(pvalue_fdr < 0.05 & estimate > 0, 1, 0),
         significant_pos_bonf = if_else(pvalue_bonferonni < 0.05 & estimate > 0, 1, 0)) %>% 
  mutate(final_dispatch_code = str_remove(column, "call_type_") %>% 
           str_replace("_", "-") %>% 
           str_to_upper()) 



# creating histogram for pvalue distribution ------------------------------

pvalue_hist <- results %>%
  mutate(positive = if_else(estimate > 0, "Postive", "Negative")) %>% 
  ggplot(aes(pvalue_fdr, fill = positive)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "dark red") +
  labs(x = "Benjamini & Hochberg (1995) P-value",
       y = "Count",
       fill = "Coefficient Estimate") +
  ggthemes::scale_fill_stata() +
  theme(legend.position = "bottom")



# annotations -------------------------------------------------------------

positive_call_types_change <- results %>% 
  filter(significant_pos == 1) %>%
  pull(final_dispatch_code)

postive_call_types_fdr_change <- results %>% 
  filter(significant_pos_fdr == 1) %>%
  pull(final_dispatch_code)


any_call_type_change_fdr <- results %>% 
  filter(pvalue_fdr < 0.05) %>% 
  pull(final_dispatch_code)


pvalue_hist <- pvalue_hist +
  annotation_custom(
  grob = grid::textGrob(
    label = paste0("Hypothesis Tests: ", nrow(results), "\n",
                   "# Tests < 0.05: ", length(any_call_type_change_fdr), "\n",
                   "# Tests < 0.05 (Positive Coefficient): ", length(postive_call_types_fdr_change)),
    x = 0.1, y = 0.8, hjust = 0, vjust = 1,
    gp = grid::gpar(col = "black", fontsize = 7, fontface = "italic")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
) +theme(panel.grid.major = element_blank())



ggsave(pvalue_hist, filename = "paper/appendix_figures/appendix_pvalue_hist.jpeg", width = 7, height = 5)

ggsave(pvalue_hist, filename = "presentations/figures/pvalue_hist.jpeg", width = 7, height = 5)
