
sst <- es_data_dispatch %>% 
  filter(never_treated == 0) %>% 
  filter(time_to_treat < 12 & time_to_treat > -12) %>% 
  rowwise() %>% 
  mutate(sst_per_dispatch = number_sst_alerts/number_dispatches, .before = 1) %>% 
  group_by(time_to_treat) %>% 
  summarize(sst_per_dispatch = mean(sst_per_dispatch, na.rm = T)) %>% 
  ungroup() 
number_sst_col %>% 
  tail()


x <- es_data_dispatch %>% 
  feols(entry_to_dispatch_1 ~ i(time_to_treat, ref = c(-1, -1000)) +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          officer_hours |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:25) %>% 
  mutate(periods = c(-12:12)) %>% 
  mutate(type = "TWFE") %>% 
  filter(periods %in% c(-11:11)) %>% 
  left_join(number_sst_col, join_by(periods == time_to_treat)) %>% 
  ggplot(aes(periods, estimate, color = type, shape = type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, color = "dark red") 


es_data_dispatch %>% 
  feols(entry_to_dispatch_1 ~ i(time_to_treat, ref = c(-1, -1000)) +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          officer_hours |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:25) %>% 
  mutate(periods = c(-12:12)) %>% 
  mutate(type = "TWFE") %>% 
  filter(periods %in% c(-11:11)) %>% 
  left_join(sst, join_by(periods == time_to_treat)) %>% 
  ggplot() +
  geom_point(aes(periods, estimate, color = type, shape = type),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(periods, estimate, ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_col(aes(x = periods, y = number_sst_alerts*1000), alpha = 0.3) +
  scale_y_continuous("d", sec.axis = sec_axis(~./1000)) +
  theme_minimal()


sst <- es_data_dispatch %>% 
  filter(never_treated == 0) %>% 
  filter(time_to_treat < 12 & time_to_treat > -12) %>% 
  group_by(time_to_treat) %>% 
  summarize(number_sst_alerts = sum(number_sst_alerts)/sum(number_dispatches_1)) %>% 
  ungroup()
sst %>% 
  mutate(number_sst_alerts * 1000) %>% View()
  