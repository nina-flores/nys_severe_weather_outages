### The objective of this script is to create an indicator of outage exposure
### the main exposure, is going to be urban, rural, and NYC specific and the 
### analyses will be conducted separately. 
### Here, for each, I will calculate their 90th percentile of outages
### and then, for each hour, will sum up the number of hours 
### exceeding that threshold in the previous 24 hours. This cumulative
### sum variable will be our main exposure.

require(tidyverse)
require(data.table)
require(dplyr)
require(sf)
require(fst)
require(lubridate)

# read in the data: 
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final")
outages <- read_fst("weather_outages_hourly.fst") %>%
  arrange(datetime_eastern)


########################################
### 1. calculate the outage threshold###
########################################


outages_90th_threshold <- outages %>%
  filter(customers_out > 0) %>% # filters out non outages
  filter(customers > 0) %>% 
  group_by(urn) %>%
  mutate(percentile_90th = quantile(prop_out, probs = .90, na.rm = TRUE))%>%
  select(percentile_90th, urn) %>%
  unique() 


########################################
### 2. identify times when above threshold
########################################

run <- list("u", "r", "n")

for(i in run){

assign(paste0("outages_",i), outages %>%
  filter(urn == i) %>%
  left_join(outages_90th_threshold)%>%
  mutate(i_gt_90pct = ifelse(prop_out>=percentile_90th,1,0)) %>%
  arrange(dps_id, datetime_eastern))

}

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/outage indicators")

outages_r$ind_90pct <- f_outage_tracker_dps_id(i_gt_90pct,outages_r)
write.fst(outages_r, "rural_outages.fst")


outages_u$ind_90pct <- f_outage_tracker_dps_id(i_gt_90pct,outages_u)
write.fst(outages_u, "urban_outages.fst")

outages_n$ind_90pct <- f_outage_tracker_dps_id(i_gt_90pct,outages_n)
write.fst(outages_n, "nyc_outages.fst")


