

### one of the analyses i can do for the missing data is as follows:
# estimate regressions for calltodispatch and calltoclose for missing
# and non-missing. 
# it looks like missing is highly correlated with higher dispatch times
# what about arrests?
# more likely to be associated without an arrest being made
# victim hurt?
## overlay trends of the event studies to show consistent changes in
## call to dispatch times
## split by officer hours missing OS data post treatment?
top <- dispatch_panel_p1 %>% 
  count(final_dispatch_description, sort = T) %>% 
  head(5) %>% 
  pull(final_dispatch_description)

dispatch_panel_p1 %>% 
  mutate(missing_os = if_else(is.na(on_scene_date), 1, 0)) %>% 
  filter(missing_os == 1) %>% 
  feols(entry_to_dispatch ~ treatment | district + date + 
          final_dispatch_description + hour)

dispatch_panel_p1 %>% 
  mutate(missing_os = if_else(is.na(on_scene_date), 1, 0)) %>% 
  filter(gun_crime_report == 1) %>% 
  feols(entry_to_dispatch ~ treatment + missing_os | district^missing_os + date^missing_os)

dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~ treatment*missing_os| district^missing_os + date^missing_os + 
          final_dispatch_description^missing_os + hour^missing_os)
dispatch_panel_p1 %>% 
  feols(entry_to_close ~ treatment*missing_os| district^missing_os + date^missing_os + 
          final_dispatch_description^missing_os + hour^missing_os)


dispatch_panel_p1 %>% 
  mutate(above_median_oh = if_else(officer_hours >= officer_hours_median,1 ,0)) %>% 
  feols(missing_os ~ treatment*above_median_oh|district^above_median_oh + date^above_median_oh + 
          final_dispatch_description^above_median_oh + hour^above_median_oh)

dispatch_panel_p1 %>% 
  feols(missing_os ~ treatment*time_sensitive_call |
          district^time_sensitive_call + date^time_sensitive_call +
          final_dispatch_description^time_sensitive_call)

dispatch_panel_p1 %>% 
  count(final_dispatch_description, missing_os) %>% 
  group_by(final_dispatch_description) %>% 
  mutate(total = sum(n),
         frac = n/total) %>% 
  filter(missing_os == 1) %>% View()
