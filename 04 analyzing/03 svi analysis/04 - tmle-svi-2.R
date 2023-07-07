
require(tidyverse)
require(data.table)
require(dplyr)
require(sf)
require(fst)
require(ltmle)
require(earth)




setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
data_u<- read.fst("sw_outages_u_any.fst")%>% na.omit() 
data_u<-data_u[,c(1,9,2,3,4,5,6,7,8)]


data_r<- read.fst("sw_outages_r_any.fst")%>% na.omit() 
data_n<- read.fst("sw_outages_n_any.fst")%>% na.omit() 

set.seed(444)

names(data_n)

run <- list("u", "r", "n")
sev_weath <- list(
                  "sum_heat_outages_bin",
                  "sum_wind_outages_bin",
                  "sum_precip_outages_bin")

quar <- list(2,3,4)



data_list <- list()

for (i in run) {
  data_i <- get(paste0("data_", i))
  
  for (q in quar) {
    data_i_q <- data_i %>%
      filter(svi_quartile == 1 | svi_quartile == q) %>%
      mutate(svi_bin = ifelse(svi_quartile == 1, 0, 1))
    
    data_list[[paste0("data_", i, "_q", q)]] <- data_i_q
  }
}


for (i in run) {
  for (sw in sev_weath) {
    for (q in quar) {
      data_i_q <- data_list[[paste0("data_", i, "_q", q)]] 
      Y <- as.vector(data_i_q[[sw]])
      A <- as.vector(data_i_q$svi_bin)
      W <- as.matrix(data_i_q$utility)
      
      a <- tmle(Y = Y, A = A, W = W, family = "binomial")
      
      output_name <- paste0("output_", i, "_", sw, "_q", q)
      assign(output_name, data.frame(
        ATE = a$estimates$ATE$psi,
        lower_CI = a$estimates$ATE$CI[1],
        upper_CI = a$estimates$ATE$CI[2],
        sev_weath = sw,
        quarter = q
      )) %>% bind_rows()
    }
  }
}
