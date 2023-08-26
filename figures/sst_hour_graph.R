library(tidyverse)

# sdsc <- c("3/1/18", "1/1/18", "1/1/18", "1/1/18","3/15/18", "1/7/17","3/1/18", "3/15/17", "3/15/17",
#           "2/17/17","3/15/17", "1/1/18")
# sdsc_districts <- c(2,3,4,5,6,7,8,9,10,11,15,25)
# 
# rollout_sdsc <- tibble(sdsc, sdsc_districts) %>% 
#   mutate(sdsc = mdy(sdsc))

sst <- read_csv("created_data/sst_dispatch_cpd.csv")


sst_by_hour <- sst %>% 
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
  geom_histogram(bins = 24, alpha = 0.8) +
  scale_x_continuous(breaks = c(0:23)) +
  theme_minimal() +
  ggthemes::scale_fill_stata() +
  labs(x = "Hour of the Day", y = "Number SST Dispatches", fill = "") +
  theme(legend.position = "bottom",  panel.grid.major =  element_blank())
