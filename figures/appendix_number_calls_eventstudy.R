

library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)
library(patchwork)


if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}


## controls
setFixest_fml(..ctrl = ~0| district + date )


agg_outcomes <- read_csv('analysis_data/xxaggregate_outcomes.csv')


aggregate_calls <- dispatch_panel %>% 
  group_by(date, district) %>% 
  summarize(number_dispatches = n()) %>% ungroup()


aggregate_p1_calls <- dispatch_panel_p1 %>% 
  group_by(date, district) %>% 
  summarize(number_dispatches_p1 = n()) %>% ungroup()

aggregate_calls_p2 <- dispatch_panel %>% 
  filter(priority_code ==2) %>% 
  group_by(date, district) %>%
  summarize(number_dispatches_p2 = n()) %>% ungroup()


aggregate_calls_p3 <- dispatch_panel %>% 
  filter(priority_code ==3) %>% 
  group_by(date, district) %>%
  summarize(number_dispatches_p3 = n()) %>% ungroup()



agg_outcomes <- agg_outcomes %>% 
  left_join(aggregate_calls) %>% 
  left_join(aggregate_p1_calls) %>% 
  left_join(aggregate_calls_p2) %>% 
  left_join(aggregate_calls_p3)


## getting rid of those days we restrict the sample to. 
agg_outcomes <- agg_outcomes %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date))

agg_outcomes <- agg_outcomes %>% 
  filter(!(day == 4 & month == 7 ) &
           !(day == 1 & month == 1) &
           !(day == 31 & month == 12))



# setting up event study --------------------------------------------------


agg_outcomes <- agg_outcomes %>% 
  mutate(time_to_treat = time_length(date - shotspot_activate,
                                     "month") %>% 
           magrittr::add(1) %>% 
           floor() %>%
           magrittr::subtract(1),
         .by = district) %>% 
  mutate(time_to_treat = case_when(
    time_to_treat > 24 ~ 24,
    time_to_treat < -12 ~ -12,
    .default = time_to_treat
  )) %>% 
  mutate(time_to_treat = if_else(is.na(time_to_treat), -1000, time_to_treat)) 


# event study graph function ----------------------------------------------

create_event_study_graphic <- function(x){
  x %>% 
  ggplot(aes(periods, estimate)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5),
                width = 0.5) +
  geom_hline(yintercept = 0, color = "dark red") +
  labs(x = "Months to Adoption",
       y = "Point Estimate and 95% Confidence Interval",
       color = "",
       shape = "") +
  theme_minimal() +
  geom_vline(aes(xintercept = -1), linetype = 'dashed') +
  ggthemes::scale_color_stata() +
  theme(legend.position = "none")
  }

pull_conf_int <- function(x){
  confint(x) %>% 
    as_tibble() %>% janitor::clean_names() %>% 
    mutate(across(everything(), ~sprintf("%.3f", .))) %>% 
    mutate(conf.int = glue::glue("[{x2_5_percent},{x97_5_percent}]")) %>% 
    pull(conf.int)
}

# calls -------------------------------------------------------------------


es_number_calls <- agg_outcomes %>% 
  fepois(number_dispatches~ i(time_to_treat, ref = c(-1, -1000)) | district + date,
         cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Poisson",
         outcome = "Panel A: Number of 911 Calls") %>% 
  create_event_study_graphic() 

did_all_calls <- agg_outcomes %>% 
  fepois(number_dispatches ~treatment|district + date,
         cluster = "district") 

did_all_calls_coef <- did_all_calls$coefficients %>% sprintf("%.3f",.)

did_all_calls_pvalue <- did_all_calls$coeftable[,4] %>% sprintf("%.3f",.)

did_all_calls_conf <- did_all_calls %>% pull_conf_int()

panel_1 <- es_number_calls +
  annotation_custom(
    grob = grid::textGrob(
      label = paste0("DiD Coefficient: ", did_all_calls_coef, "\n",
                     "Conf.Int: ", did_all_calls_conf, "\n",
                     "P-value: ", did_all_calls_pvalue),
      x = 0.02, y = 0.02, hjust = 0, vjust = 0, 
      gp = grid::gpar(col = "black", fontsize = 5, fontface = "italic")
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  labs(title = "Panel A: Number of All Calls") +
  theme(plot.title = element_text(size = 8),
        panel.grid.major = element_blank())
  






es_number_calls_p1 <- agg_outcomes %>% 
  fepois(number_dispatches_p1~ i(time_to_treat, ref = c(-1, -1000)) | district + date,
         cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Poisson",
         outcome = "Panel B: Number of Priority 1 911 Calls") %>% 
  create_event_study_graphic() 


did_p1_calls <- agg_outcomes %>% 
  fepois(number_dispatches_p1 ~treatment|district + date,
         cluster = "district") 

did_p1_calls_coef <- did_p1_calls$coefficients %>% sprintf("%.3f",.)

did_p1_calls_pvalue <- did_p1_calls$coeftable[,4] %>% sprintf("%.3f",.)

did_p1_calls_conf <- did_p1_calls %>% pull_conf_int()


panel_2 <- es_number_calls_p1 +
  annotation_custom(
    grob = grid::textGrob(
      label = paste0("DiD Coefficient: ", did_p1_calls_coef, "\n",
                     "Conf.Int: ", did_p1_calls_conf, "\n",
                     "P-value: ", did_p1_calls_pvalue),
      x = 0.02, y = 0.02, hjust = 0, vjust = 0, 
      gp = grid::gpar(col = "black", fontsize = 5, fontface = "italic")
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )+
  labs(title = "Panel B: Number of Priority 1 Calls") +
  theme(plot.title = element_text(size = 8),
        panel.grid.major = element_blank())




es_number_calls_p2 <- agg_outcomes %>% 
  fepois(number_dispatches_p2~ i(time_to_treat, ref = c(-1, -1000)) | district + date,
         cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Poisson",
         outcome = "Panel C: Number of Priority 2 911 Calls") %>% 
  create_event_study_graphic()

did_p2_calls <- agg_outcomes %>% 
  fepois(number_dispatches_p2 ~treatment|district + date,
         cluster = "district")

did_p2_calls_coef <- did_p2_calls$coefficients %>% sprintf("%.3f",.)

did_p2_calls_pvalue <- did_p2_calls$coeftable[,4] %>% sprintf("%.3f",.)

did_p2_calls_conf <- did_p2_calls %>% pull_conf_int()

panel_3 <- es_number_calls_p2 +
  annotation_custom(
    grob = grid::textGrob(
      label = paste0("DiD Coef: ", did_p2_calls_coef, "\n",
                     "Conf.Int: ", did_p2_calls_conf, "\n",
                     "P-value: ", did_p2_calls_pvalue),
      x = 0.02, y = 0.02, hjust = 0, vjust = 0, 
      gp = grid::gpar(col = "black", fontsize = 5, fontface = "italic")
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  labs(title = "Panel C: Number of Priority 2 Calls") +
  theme(plot.title = element_text(size = 8),
        panel.grid.major = element_blank())

es_number_calls_p3 <- agg_outcomes %>% 
  fepois(number_dispatches_p3~ i(time_to_treat, ref = c(-1, -1000)) | district + date,
         cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Poisson",
         outcome = "Panel D: Number of Priority 3 911 Calls") %>% 
  create_event_study_graphic()

did_p3_calls <- agg_outcomes %>% 
  fepois(number_dispatches_p3 ~treatment|district + date,
         cluster = "district")

did_p3_calls_coef <- did_p3_calls$coefficients %>% sprintf("%.3f",.)

did_p3_calls_pvalue <- did_p3_calls$coeftable[,4] %>% sprintf("%.3f",.)

did_p3_calls_conf <- did_p3_calls %>% pull_conf_int()

panel_4 <- es_number_calls_p3 +
  annotation_custom(
    grob = grid::textGrob(
      label = paste0("DiD Coef: ", did_p3_calls_coef, "\n",
                     "Conf.Int: ", did_p3_calls_conf, "\n",
                     "P-value: ", did_p3_calls_pvalue),
      x = 0.02, y = 0.8, hjust = 0, vjust = 0, 
      gp = grid::gpar(col = "black", fontsize = 5, fontface = "italic")
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  labs(title = "Panel D: Number of Priority 3 Calls") +
  theme(plot.title = element_text(size = 8),
        panel.grid.major = element_blank())


number_calls_panels <- panel_1 + panel_2 + 
  panel_3 + panel_4 +
  patchwork::plot_layout(axis_titles = 'collect') 


ggsave(number_calls_panels, filename = "paper/figures/number_calls_panels.jpeg", width = 7, height = 5)
