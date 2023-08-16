library(tidyverse)



sst <- read_csv("created_data/sst_dispatch_cpd.csv")


sst %>% 
  mutate(date = as_date(entry_received_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)),
         hour = hour(entry_received_date)) %>% 
  mutate(watch = case_when(
    hour < 8 ~ "Watch 1",
    hour < 16 & hour >=8 ~ "Watch 2",
    hour >= 16 ~ "Watch 3")) %>% 
  ggplot(aes(hour, fill = watch)) +
  geom_histogram(bins = 24) +
  scale_x_continuous(breaks = c(0:23)) +
  theme_minimal() +
  ggthemes::scale_fill_stata() +
  labs(x = "Hour", y = "Number SST Dispatches", fill = "Shift") +
  theme(legend.position = "bottom")
