library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)
library(fwildclusterboot)

set.seed(1992)
dqrng::dqset.seed(1992)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

## to make this bootstrap work, several steps need to be taken:
## 1. Date must be a factor. You should change to as.integer(date) in the data set
## 2. You cannot pipe into the boottest function, because it cannot recognize objects piped

## need this for body worn camera implementation
bwc <- read_csv("created_data/bwc_rollout.csv") %>% 
  mutate(bwc_date = mdy(bwc_date),
         sdsc_max = mdy(sdsc_max))



dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  left_join(bwc) %>% 
  mutate(treatment_bwc = if_else(date >= bwc_date, 1, 0),
         treatment_bwc = if_else(is.na(treatment_bwc), 0, treatment_bwc),
         treatment_sdsc = if_else(date >=sdsc_max, 1, 0),
         treatment_sdsc = if_else(is.na(treatment_sdsc), 0, treatment_sdsc),
         .by = district)




# for bootstrap to work ---------------------------------------------------


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(date = as.integer(date))

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(final_dispatch_description_1 = match(final_dispatch_description,unique(final_dispatch_description)))


setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description_1 + hour)


# Panel A: call to dispatch --------------------------------------------------------


sdsc_d1 <- 
  feols(entry_to_dispatch ~treatment + treatment_sdsc + ..ctrl,
        data = dispatch_panel_p1)


sdsc_d2 <- 
  feols(entry_to_dispatch ~treatment + treatment_sdsc + ..ctrl,
        data = dispatch_panel_p1 %>% 
          filter(!district %in% c(7,9)))


bwc_d1 <- 
  feols(entry_to_dispatch ~treatment + treatment_bwc + ..ctrl,
        data = dispatch_panel_p1)




# Panel B: call to on-scene --------------------------------------------------------


sdsc_o1 <- 
  feols(entry_to_onscene ~treatment + treatment_sdsc + ..ctrl,
        data = dispatch_panel_p1)


sdsc_o2 <-  
  feols(entry_to_onscene ~treatment + treatment_sdsc + ..ctrl,
        data = dispatch_panel_p1 %>% 
          filter(!district %in% c(7,9)))


bwc_o1 <- 
  feols(entry_to_onscene ~treatment + treatment_bwc + ..ctrl,
        data = dispatch_panel_p1)


# Panel C: arrests --------------------------------------------------------


sdsc_a1 <- feols(arrest_made*100 ~treatment + treatment_sdsc + ..ctrl,
        data = dispatch_panel_p1)


sdsc_a2 <- feols(arrest_made*100 ~treatment + treatment_sdsc + ..ctrl,
        data = dispatch_panel_p1 %>% 
          filter(!district %in% c(7,9)))


bwc_a1 <- feols(arrest_made*100 ~treatment + treatment_bwc + ..ctrl,
        data = dispatch_panel_p1)




# bootstraps --------------------------------------------------------------


sdsc_a1_b <- 
  boottest(sdsc_a1, clustid = c("district"), 
           B = 1000, 
           param = "treatment",
           fe = c("date"))

sdsc_a2_b <- boottest(sdsc_a2, clustid = c("district"), 
                     B = 999, 
                     param = "treatment",
                     fe = c("date"))

bwc_a1_b <- boottest(bwc_a1, clustid = c("district"), 
                     B = 999, 
                     param = "treatment",
                     fe = c("date"))

sdsc_o_1 <- 
  boottest(sdsc_o1, clustid = c("district"), 
           B = 999, 
           param = "treatment",
           fe = c("date"))
sdsc_o_2 <- 
  boottest(sdsc_o2, clustid = c("district"), 
           B = 999, 
           param = "treatment",
           fe = c("date"))
bwc_o_3 <- 
  boottest(bwc_o1, clustid = c("district"), 
           B = 999, 
           param = "treatment",
           fe = c("date"))

sdsc_a_1b <- boottest(sdsc_a1, clustid = c("district"), 
                      B = 999, 
                      param = "treatment",
                      fe = c("date"))

sdsc_a_1b <- boottest(sdsc_a2, clustid = c("district"), 
                      B = 999, 
                      param = "treatment",
                      fe = c("date"))

bwc_a_1b <- boottest(bwc_a1, clustid = c("district"), 
                      B = 999, 
                      param = "treatment",
                      fe = c("date"))



c("0.006", "0.004", "0.010",
  "0.002","0.001","0.002",
  "0.002","0.009","0.003" )
