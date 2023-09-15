

library(fwildclusterboot)

set.seed(1992)
dqrng::dqset.seed(1992)


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(date = as.integer(date))

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(final_dispatch_description_1 = match(final_dispatch_description,unique(final_dispatch_description)))
arrest_rate <- 
  feols(arrest_made ~ treatment | district + date +
           hour + final_dispatch_description_1, data = dispatch_panel_p1)

arrest_rate_gun <- 
  feols(arrest_made ~ treatment | district + date +
          hour + final_dispatch_description_1, data = dispatch_panel_p1 %>% 
          filter(gun_crime_report == 1))

arrest_rate_no_gun <-  
  feols(arrest_made ~ treatment  | district + date +
          hour + final_dispatch_description_1, data = dispatch_panel_p1 %>% 
          filter(gun_crime_report != 1) )

arrest_rate_domestic_bat <- 
  feols(arrest_made ~ treatment |district + date +
          hour , data = dispatch_panel_p1 %>% 
          filter(final_dispatch_description == "DOMESTIC BATTERY"))

arrest_rate_domestic_disturb <- 
  feols(arrest_made ~ treatment |district + date +
          hour , data = dispatch_panel_p1 %>% 
          filter(final_dispatch_description == "DOMESTIC DISTURBANCE"))

arrest_rate_battery <-  
  feols(arrest_made ~ treatment |district + date +
          hour , data = dispatch_panel_p1 %>% 
          filter(final_dispatch_description == "BATTERY IP"))


arrest_1 <- boottest(arrest_rate, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = c("date"))

arrest_2 <- boottest(arrest_rate_gun, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")

arrest_25 <- boottest(arrest_rate_no_gun, clustid = c("district"), 
                     B = 1499, 
                     param = "treatment",
                     fe = "date")

arrest_3 <- boottest(arrest_rate_domestic_bat, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")

arrest_4 <- boottest(arrest_rate_domestic_disturb, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")

arrest_5 <- boottest(arrest_rate_battery, clustid = c("district"), 
                      B = 999, 
                      param = "treatment",
                      fe = c("date"))

wild_bootstrap_arrest <- c('0.001', '0.412','0.003', '0.003', '0.049', 0.109)


## full sample of time-sensitive calls
victim_1 <- 
  feols(victim_injury ~ treatment | district + date +
          hour + final_dispatch_description_1, data = dispatch_panel_p1)

## time sensitive call with gun is person with a gun or shots fired
victim_gun <- 
  feols(victim_injury ~ treatment | district + date +
          hour + final_dispatch_description_1, data = dispatch_panel_p1 %>% 
          filter(gun_crime_report == 1))

## non-time sensitive stuff is everything else that can be put into the table
victim_no_gun <- feols(victim_injury ~ treatment | district + date +
                         hour + final_dispatch_description_1, data = dispatch_panel_p1 %>% 
                         filter(gun_crime_report == 0))


victim_1 <- boottest(victim_1, clustid = c("district"), 
                     B = 999, 
                     param = "treatment",
                     fe = c("date"))

victim_2 <- boottest(victim_gun, clustid = c("district"), 
                     B = 999, 
                     param = "treatment",
                     fe = "date")

victim_3 <- boottest(victim_no_gun, clustid = c("district"), 
                      B = 999, 
                      param = "treatment",
                      fe = "date")

c('0.245','0.067', '0.895')