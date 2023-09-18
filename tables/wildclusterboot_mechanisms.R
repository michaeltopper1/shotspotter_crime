library(fwildclusterboot)

set.seed(1992)
dqrng::dqset.seed(1992)
## to make this bootstrap work, several steps need to be taken:
## 1. Date must be a factor. You should change to as.integer(date) in the data set
## 2. You cannot pipe into the boottest function, because it cannot recognize objects piped

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(date = as.integer(date))

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(final_dispatch_description_1 = match(final_dispatch_description,unique(final_dispatch_description)))


# Panel A -----------------------------------------------------------------


ed_preferred_1 <- feols(entry_to_dispatch ~ treatment | district + date +
                          final_dispatch_description_1 + hour,
                        cluster = ~district,
                        data = dispatch_panel_p1)

sst_ed_1 <- feols(entry_to_dispatch ~ number_sst_dispatches | district + date +
                    final_dispatch_description_1 + hour,
                  cluster = ~district,
                  data = dispatch_panel_p1 %>% 
                    filter(treatment == 1 | never_treated == 1))

ed_above_med <- dispatch_panel_p1 %>%
  filter(officer_hours > officer_hours_median) %>%
  feols(entry_to_dispatch ~ treatment |district + date +
          final_dispatch_description + hour,
        cluster = ~district)


sst_os_1 <- feols(entry_to_onscene ~ number_sst_dispatches | district + date +
                    final_dispatch_description_1 + hour,
                  cluster = ~district,
                  data = dispatch_panel_p1 %>% 
                    filter(treatment == 1 | never_treated == 1) )

sst_os_above_med <-  
  feols(entry_to_onscene ~ number_sst_dispatches |district + date +
          final_dispatch_description_1 + hour,
        cluster = ~district,
        data = dispatch_panel_p1 %>%
          filter(treatment == 1 | never_treated == 1) %>% 
          mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
          filter(officer_hours > officer_hours_median))

sst_os_below_med <-  
  feols(entry_to_onscene ~ number_sst_dispatches |district + date +
          final_dispatch_description_1 + hour,
        cluster = ~district,
        dispatch_panel_p1 %>%
          filter(treatment == 1 | never_treated == 1) %>% 
          mutate(officer_hours_median = median(officer_hours, na.rm = T), .by = district) %>% 
          filter(officer_hours <= officer_hours_median))


# extensive bootstraps ----------------------------------------------------


mechanism_1 <- 
  boottest(ed_preferred_1, clustid = c("district"), 
           B = 999, 
           param = "treatment",
           fe = c("date"))

mechanism_2 <-  boottest(sst_ed_1, clustid = c("district"), 
                         B = 999, 
                         param = "treatment",
                         fe = c("date"))
mechanism_3 <-  boottest(sst_ed_below_med, clustid = c("district"), 
                         B = 999, 
                         param = "treatment",
                         fe = c("date"))


# intensive bootstraps ----------------------------------------------------



mechanism_4 <-  boottest(sst_os_1, clustid = c("district"), 
                         B = 999, 
                         param = "treatment",
                         fe = c("date"))


mechanism_5 <-  boottest(sst_os_above_med, clustid = c("district"), 
                         B = 999, 
                         param = "treatment",
                         fe = c("date"))

mechanism_6 <-  boottest(sst_os_below_med, clustid = c("district"), 
                         B = 999, 
                         param = "treatment",
                         fe = c("date"))
