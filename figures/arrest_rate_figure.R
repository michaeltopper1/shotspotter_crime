

outcomes <- c("Overall Change",
  "Domestic Battery",
  "Domestic Disturbance",
  "Robbery",
  "EMS",
  "Battery")

# outcomes <- c("arrest_rate_1",
#               "domestic_battery_p1_arrestrate",
#               "domestic_disturb_p1_arrestrate",
#               "robbery_jo_p1_arrestrate",
#               "ems_p1_arrestrate",
#               "battery_ip_p1_arrestrate",
#               "assault_ip_p1_arrestrate",
#               "shots_fired_p1_arrestrate",
#               "check_well_p1_arrestrate",
#               "fire_p1_arrestrate",
#               "person_gun_arrestrate")

arrest_rates <- feols(c(arrest_rate_1,
          domestic_battery_p1_arrestrate,
          domestic_disturb_p1_arrestrate,
          robbery_jo_p1_arrestrate,
          ems_p1_arrestrate,
          battery_ip_p1_arrestrate) ~treatment  +
          number_dispatches_1 + number_dispatches_2 +
          number_dispatches_3 +
          officer_hours|district + date,
        data= dispatch_panel) 


arrest_estimates <- map_dfr(arrest_rates, ~broom::tidy(.,conf.int = T)) %>% 
  filter(term == "treatment") %>% 
  bind_cols("outcomes" =outcomes) %>% 
  mutate(facet_arrest = if_else(outcomes == "Overall Change", "Arrest Rate:\nAggregate Change", "Arrest Rate:\nChange by Most Frequent Types"))

arrest_means <- map_dfr(arrest_rates, ~as.numeric(fitstat(., type = "my"))) %>% t() %>% 
  as_tibble() %>% 
  rename("mean" = V1)

arrest_estimates <- arrest_estimates %>% 
  bind_cols(arrest_means) %>% 
  mutate(percentage = estimate/mean, .before = 1) %>% 
  mutate(percentage = percentage %>% scales::percent())

arrest_estimates <- arrest_estimates %>% 
  mutate(arrest_rate = if_else(outcomes == "Arrest Rate", "Percentage\nChange: -4.95%", "")) %>% 
  mutate(outcomes = glue::glue("{outcomes} ({percentage})"))

arrest_estimates %>% 
  mutate(outcomes = fct_reorder(outcomes, row_number())) %>% 
  ggplot(aes(arrest_rate, estimate, color = outcomes, shape = outcomes)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  fatten = 4,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Point Estimate and 95% Confidence Interval", 
       color = "Arrest Types", shape = "Arrest Types") +
  ggthemes::scale_color_stata() +
  facet_wrap(~facet_arrest, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_line(color = "grey",lineend = "square"))


