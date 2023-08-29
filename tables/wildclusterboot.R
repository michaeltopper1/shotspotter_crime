library(fwildclusterboot)

set.seed(1992)
dqrng::dqset.seed(1992)
## to make this bootstrap work, several steps need to be taken:
## 1. Date must be a factor. You should change to as.integer(date) in the data set
## 2. You cannot pipe into the boottest function, because it cannot recognize objects piped

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(date = as.integer(date))


dispatch_1 <- boottest(entry_d, clustid = c("district"), 
              B = 999, 
              param = "treatment",
          fe = c("date"))

dispatch_2 <- boottest(entry_d_1, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")
dispatch_3 <- boottest(entry_d_2, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")

dispatch_4 <- boottest(entry_d_4, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")
  
onscene_1 <- boottest(entry_os, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = c("date"))

onscene_2 <- boottest(entry_os_1, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")
onscene_3 <- boottest(entry_os_2, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")

onscene_4 <- boottest(entry_os_4, clustid = c("district"), 
                       B = 999, 
                       param = "treatment",
                       fe = "date")
  