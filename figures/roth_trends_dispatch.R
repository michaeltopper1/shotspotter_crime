##
library(HonestDiD)
library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(modelsummary)
library(did2s)

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}


# creating the event study data -------------------------------------------

es_data_dispatch <- dispatch_panel %>% 
  mutate(time_to_treat = time_length(as_date(date) - shotspot_activate,
                                     "month") %>% 
           magrittr::add(1) %>% 
           floor() %>%
           magrittr::subtract(1),
         .by = district) %>% 
  mutate(time_to_treat = case_when(
    time_to_treat > 12 ~ 12,
    time_to_treat < -12 ~ -12,
    .default = time_to_treat
  )) %>% 
  mutate(time_to_treat = if_else(is.na(time_to_treat), -1000, time_to_treat)) 



# estimating 2sdid and TWFE -----------------------------------------------

## TWFE
twfe_dispatch <- es_data_dispatch %>% 
  feols(entry_to_dispatch_1 ~ i(time_to_treat, ref = c(-1, -1000)) +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          number_dispatches_0 +
          officer_hours |district + date,
        cluster = ~district, data = .)

## this is omitting the binned endpoints and controls.
## Although there is no standard for this, we believe this is the right thing to do
## results are consistent without doing this as well
twfe_dispatch_coef <- twfe_dispatch$coefficients[2:23]

twfe_dispatch_cov <- twfe_dispatch$cov.scaled[2:23,2:23]

##34.3 point estimate average
# roth: testing period 5 after implementation -----------------------------


twfe_dispatch_original <- HonestDiD::constructOriginalCS(betahat = twfe_dispatch_coef,
                                                    sigma = twfe_dispatch_cov,
                                                    numPrePeriods = 11,
                                                    numPostPeriods = 11,
                                                    l_vec = rep(1/11, 11))
twfe_dispatch_honest <- HonestDiD::createSensitivityResults(betahat = twfe_dispatch_coef,
                                                sigma = twfe_dispatch_cov,
                                                numPrePeriods = 11,
                                                numPostPeriods = 11,
                                                Mvec = seq(from = 0, to = 0.5, by =0.1),
                                                l_vec = rep(1/11, 11))


sensitivity_plot_dispatch <- createSensitivityPlot(twfe_dispatch_honest, twfe_dispatch_original) +
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4)) +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  labs(color = "") +
  theme(legend.position = "bottom")

## 2sDID

# did2s_dispatch <- did2s(es_data_dispatch,
#            yname = "entry_to_dispatch_1",
#            first_stage = ~number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
#              number_dispatches_0 + officer_hours | district + date,
#            second_stage = ~ i(time_to_treat, ref = c(-1, -1000)),
#            treatment = "treatment",
#            cluster_var = "district")
# 
# did2s_dispatch_coef <- did2s_dispatch$coefficients[1:24]
# 
# did2s_dispatch_cov <- did2s_dispatch$cov.scaled[1:24,1:24]
# 
# 
# did2s_dispatch_original <- HonestDiD::constructOriginalCS(betahat = did2s_dispatch_coef,
#                                                          sigma = did2s_dispatch_cov,
#                                                          numPrePeriods = 16,
#                                                          numPostPeriods = 8,
#                                                          l_vec = rep(1/8, 8))
# did2s_dispatch_honest <- HonestDiD::createSensitivityResults(betahat = did2s_dispatch_coef,
#                                                             sigma = did2s_dispatch_cov,
#                                                             numPrePeriods = 16,
#                                                             numPostPeriods = 8,
#                                                             Mvec = seq(from = 0, to = 2, by =0.5),
#                                                             l_vec = rep(1/8, 8))
# 
# sensitivity_plot_dispatch_2s <- createSensitivityPlot(did2s_dispatch_honest, did2s_dispatch_original) +
#   scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2)) +
#   ggthemes::scale_color_stata() +
#   theme_minimal() +
#   labs(color = "") +
#   theme(legend.position = "bottom")



# call to on-scene --------------------------------------------------------

## TWFE
twfe_os <- es_data_dispatch %>% 
  feols(entry_to_onscene_1 ~ i(time_to_treat, ref = c(-1, -1000)) +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          number_dispatches_0 +
          officer_hours |district + date,
        cluster = ~district, data = .)

twfe_os_coef <- twfe_os$coefficients[2:23]

twfe_os_cov <- twfe_os$cov.scaled[2:23,2:23]


# roth: testing period 5 after implementation -----------------------------

## doing the first period of significant and the highest period (month 6)
## work nicely
twfe_os_original <- HonestDiD::constructOriginalCS(betahat = twfe_os_coef,
                                                         sigma = twfe_os_cov,
                                                         numPrePeriods = 11,
                                                         numPostPeriods = 11,
                                                    l_vec = rep(1/11, 11))
twfe_os_honest <- HonestDiD::createSensitivityResults(betahat = twfe_os_coef,
                                                            sigma = twfe_os_cov,
                                                            numPrePeriods = 11,
                                                            numPostPeriods = 11,
                                                            Mvec = seq(from = 0, to = 0.5, by =0.1),
                                                      l_vec = rep(1/11, 11))


sensitivity_plot_os <- createSensitivityPlot(twfe_os_honest, twfe_os_original) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2)) +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  labs(color = "") +
  theme(legend.position = "bottom")


## 2sdid

# 
# did2s_os <- did2s(es_data_dispatch,
#                         yname = "entry_to_onscene_1",
#                         first_stage = ~number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
#                           number_dispatches_0 + officer_hours | district + date,
#                         second_stage = ~ i(time_to_treat, ref = c(-1, -1000)),
#                         treatment = "treatment",
#                         cluster_var = "district")
# 
# did2s_os_coef <- did2s_os$coefficients[1:24]
# 
# did2s_os_cov <- did2s_os$cov.scaled[1:24,1:24]
# 
# 
# did2s_os_original <- HonestDiD::constructOriginalCS(betahat = did2s_os_coef,
#                                                           sigma = did2s_os_cov,
#                                                           numPrePeriods = 16,
#                                                           numPostPeriods = 8,
#                                                     l_vec = rep(1/8, 8))
# did2s_os_honest <- HonestDiD::createSensitivityResults(betahat = did2s_os_coef,
#                                                              sigma = did2s_os_cov,
#                                                              numPrePeriods = 16,
#                                                              numPostPeriods = 8,
#                                                        Mvec = seq(from = 0, to = 0.5, by =0.1),
#                                                        l_vec = rep(1/8, 8))
# 
# sensitivity_plot_os_2s <- createSensitivityPlot(did2s_os_honest, did2s_os_original) +
#   scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2)) +
#   ggthemes::scale_color_stata() +
#   theme_minimal() +
#   labs(color = "") +
#   theme(legend.position = "bottom")
