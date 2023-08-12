library(fwildclusterboot)

set.seed(1992)
dqrng::dqset.seed(1992)
## to make this bootstrap work, several steps need to be taken:
## 1. Date must be a factor. You should change to factor(date) in the data set
## 2. You cannot pipe into the boottest function, because it cannot recognize objects piped

entry_3 <-did2s(data = d,
                 yname = "entry_to_dispatch_1",
                 first_stage = ~officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 + number_dispatches_0|district,
                 second_stage = ~treatment,
                 treatment = "treatment",
                 cluster_var = "district")
static <- did2s(df_het,
                yname = "dep_var", first_stage = ~ 0 | state + year,
                second_stage = ~ i(treat, ref = FALSE), treatment = "treat",
                cluster_var = "state"
)


boottest(static, clustid = c("state"), B = 9999, param = "treat")
entry_3$call$data <- d
entry_4 <- 
  feols(entry_to_dispatch_1 ~ treatment + officer_hours +
                   number_dispatches_1 + number_dispatches_2 +
                   number_dispatches_3 + number_dispatches_0 + shotspot_border_treatment| 
                   district + date,
                 cluster = ~district, d)
entry_3_boot <- boottest(entry_3, clustid = c("district"), B = 9999, param = "treatment")
