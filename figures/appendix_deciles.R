
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


setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)

# 
# dispatch_panel_p1 %>% 
#   distinct(number_sst_dispatches, district, date) %>% 
#   filter(number_sst_dispatches != 0) %>% 
#   pull(number_sst_dispatches) %>%
#   quantile(c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
#   count(number_sst_dispatches) %>%
#   ggplot(aes(number_sst_dispatches, n))+
#   geom_col()

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(sst_0 = if_else(number_sst_dispatches == 0, 1, 0),
         sst_1 = if_else(number_sst_dispatches > 0 & 
                           number_sst_dispatches <= 1, 1, 0),
         sst_2 = if_else(number_sst_dispatches > 1 & 
                           number_sst_dispatches <=3, 1, 0), 
         sst_3 = if_else(number_sst_dispatches > 3 & 
                           number_sst_dispatches <=4, 1, 0),
         sst_4 = if_else(number_sst_dispatches > 4 & 
                           number_sst_dispatches <= 5, 1, 0),
         sst_5 = if_else(number_sst_dispatches > 5 & 
                           number_sst_dispatches <=6, 1, 0),
         sst_6 = if_else(number_sst_dispatches > 6 & 
                           number_sst_dispatches <=7, 1, 0),
         sst_7 = if_else(number_sst_dispatches > 7 & 
                           number_sst_dispatches <= 9, 1, 0),
         sst_8 = if_else(number_sst_dispatches > 9 & 
                           number_sst_dispatches <= 11, 1, 0),
         sst_9 = if_else(number_sst_dispatches > 11, 1, 0))


decile_dispatch <- dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~ sst_1 + sst_2 + sst_3 + 
          sst_4 + sst_5 + sst_6 + sst_7 + sst_8 + sst_9 + number_dispatches + ..ctrl) %>% 
  broom::tidy(conf.int = T)

decile_onscene <- dispatch_panel_p1 %>% 
  feols(entry_to_onscene ~ sst_1 + sst_2 + sst_3 + 
          sst_4 + sst_5 + sst_6 + sst_7 + sst_8 + sst_9 + number_dispatches + ..ctrl) %>% 
  broom::tidy(conf.int = T)


decile_dispatch <- decile_dispatch %>%
  slice(-10) %>% 
  mutate(decile = c("0.10\n(1)", "0.20\n(2)", "0.30\n(3)", "0.40\n(4)",
                    "0.50\n(5)", "0.60\n(6)", "0.70\n(7-9)", "0.80\n(9-11)",
                    "0.90\n(11+)")) %>% 
  mutate(decile = factor(decile, levels = c("0.10\n(1)", "0.20\n(2)", "0.30\n(3)", "0.40\n(4)",
                                            "0.50\n(5)", "0.60\n(6)", "0.70\n(7-9)", "0.80\n(9-11)",
                                            "0.90\n(11+)"))) %>% 
  mutate(outcome = "Call-to-Dispatch")



decile_onscene <- decile_onscene %>% 
  slice(-10) %>% 
  mutate(decile = c("0.10\n(1)", "0.20\n(2)", "0.30\n(3)", "0.40\n(4)",
                    "0.50\n(5)", "0.60\n(6)", "0.70\n(7-9)", "0.80\n(9-11)",
                    "0.90\n(11+)")) %>% 
  mutate(decile = factor(decile, levels = c("0.10\n(1)", "0.20\n(2)", "0.30\n(3)", "0.40\n(4)",
                                            "0.50\n(5)", "0.60\n(6)", "0.70\n(7-9)", "0.80\n(9-11)",
                                            "0.90\n(11+)"))) %>% 
  mutate(outcome = "Call-to-On-Scene")




decile_figure <- decile_dispatch %>% 
  rbind(decile_onscene) %>% 
  ggplot(aes(decile, estimate, color = outcome)) +
  geom_point() +
  geom_line(aes(group = outcome)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~outcome, ncol = 1) +
  labs(x = "Decile\n(Number of District-Day ShotSpotter Dispatches)",
       y = "Point Estimate and 95% Confidence Interval",
       color = "") +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  theme(legend.position = "bottom")
  


ggsave(decile_figure, filename = "paper/appendix_figures/decile_figure.jpeg",
       width = 7, height = 7)
