priorities <- dispatch_panel %>% 
  filter(priority_code > 0) %>% 
  count(priority_code, sort = T) %>% 
  mutate(priority_code = fct_reorder(as.factor(priority_code), n))


priority_description = c("involves an imminent threat to life, bodily injury, or major property damage/loss",
                         "in which timely police action which has the potential to affect the outcome of an incident",
                         "a response to a call for service that does not involve an imminent threat to life, bodily injury, or major property damage/loss, and a reasonable delay in police action will not affect the outcome of the incident.")

priority_fractions <- priorities %>% 
  mutate(priority_code = glue::glue("Priority {priority_code}")) %>%
  mutate(priority_code = fct_reorder(as.factor(priority_code), n)) %>%
  mutate(total = sum(n),
         fraction = round(n/total, 3) %>% scales::percent()) %>% 
  mutate(n_comma = n %>% scales::comma()) %>% 
  ggplot(aes(priority_code, n, fill = priority_code)) +
  geom_col(width = .3, alpha = 0.8) +
  geom_text(aes(label = fraction),hjust = -.1) +
  coord_flip() +
  scale_y_continuous(label= scales::comma, expand = expand_scale(mult = c(0, 0.2))) +
  ggthemes::scale_fill_wsj() +
  labs(x = "", y = "", title = "911 Calls Resulting in Police Dispatch",
       subtitle = "Chicago (2016-2022)") +
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major =  element_blank())

ggsave(priority_fractions, file = "presentations/figures/priority_fractions.jpeg",
       height = 5, width = 5)

