

library(readxl)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}


theme_set(theme_minimal())
cjars <- readxl::read_excel("created_data/cjars_toc/cjars_toc.xlsx",
                            sheet = "TOC_Result")
cjars_desc <- readxl::read_excel('created_data/cjars_toc/cjars_toc.xlsx',
                                 sheet = "UCCS_Schema")

cjars <- cjars %>% 
  left_join(cjars_desc) %>% 
  select(final_dispatch_description, uccs_code,charge_desc, offense_category_desc)


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  left_join(cjars)

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  fastDummies::dummy_cols('uccs_code')

dispatch_panel_p1 %>% 
  count(uccs_code, sort = T) %>% View()

columns_to_estimate <- dispatch_panel_p1 %>% 
  janitor::clean_names() %>% 
  select(matches("uccs_code_")) %>% 
  colnames()

plan(multisession, workers =6)
options(future.globals.maxSize = 8 * 1024^3)  # Set to 8 GB

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


# Close parallel workers
plan(sequential)


# binding -----------------------------------------------------------------

results <- results %>%
  bind_cols(result_column) %>%
  bind_cols(instances_in_data) %>%
  bind_cols(estimates_columns) %>% 
  bind_cols(conf_low, conf_high, mean_y)

results <- results %>% 
  mutate(significant = if_else(pvalue < 0.05, 1, 0),
         uccs_code = str_extract(column, "\\d{4}") %>% 
           as.double())

uccs_code_sig <- results %>% 
  filter(significant == 1) %>% pull(uccs_code)
uccs_code_sig_pos <- results %>% 
  filter(significant == 1 & estimate > 0) %>% pull(uccs_code)

dispatch_panel_p1 %>% 
  mutate(significant_uccs = if_else(uccs_code %in% uccs_code_sig, 1, 0),
         significant_uccs_pos_coef = if_else(uccs_code %in% uccs_code_sig_pos,
                                             "Significant: More Calls", "No Change or Negative")) %>% 
  select(significant_uccs_pos_coef, entry_to_dispatch,
         entry_to_onscene, entry_to_close) %>% 
  datasummary_balance(~significant_uccs_pos_coef,
                      data = .,
                      stars = c("*" = .1, "**" = .05, "***" = .01),
                      dinm_statistic = "p.value")
  


# does sst change the fraction of call types ------------------------------

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  group_by(district, date) %>% 
  mutate(total_calls = n()) %>% 
  mutate(across(starts_with('uccs_'), ~ ./total_calls)) %>% 
  ungroup()


dispatch_panel_p1 %>% 
  feols(uccs_code_1060 ~ treatment|district + date + hour)

  

