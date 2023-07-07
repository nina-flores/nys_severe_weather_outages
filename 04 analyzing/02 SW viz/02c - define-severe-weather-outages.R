# create the indicator for the extreme weather-driven outage exposure:

## for the start time of each outage - need to create an indicator to determine
## whether the outage follows extremely cold temperatures by >=24 hours

require(tidyverse)
require(data.table)
require(dplyr)
require(sf)
require(fst)
require(lubridate) 
require(runner)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/outage indicators")
swm <- read.csv("severe_metrics.csv")

outages_r <- read_fst("rural_outages.fst") %>%
  mutate(heat = swm$hot,
         cold = swm$cold,
         precip = swm$precip,
         wind = swm$wind,
         snow = swm$snow)
outages_u <- read_fst("urban_outages.fst")%>%
  mutate(heat = swm$hot,
         cold = swm$cold,
         precip = swm$precip,
         wind = swm$wind,
         snow = swm$snow)
outages_n <- read_fst("nyc_outages.fst")%>%
  mutate(heat = swm$hot,
         cold = swm$cold,
         precip = swm$precip,
         wind = swm$wind,
         snow = swm$snow)



run <- list("u", "r", "n")

for(i in run){
  
  assign(paste0("outages_",i,"_defined"), get(paste0('outages_',i)) %>%
           group_by(dps_id) %>%
           arrange(datetime_eastern) %>%
           mutate(start_outage = i_gt_90pct - lag(i_gt_90pct, default = first(i_gt_90pct)))%>%
           mutate(extreme_cold_ind = ifelse(temperature <= cold, 1,0)) %>%
           mutate(hours_exposed_24_cold = runner(extreme_cold_ind, k=9, sum)) %>%
           mutate(cold_driven_outage = ifelse(start_outage == 1 & hours_exposed_24_cold >=1,1,0))%>%
           mutate(extreme_heat_ind = ifelse(temperature >= heat, 1,0)) %>%
           mutate(hours_exposed_24_heat = runner(extreme_heat_ind, k=9, sum)) %>%
           mutate(heat_driven_outage = ifelse(start_outage == 1 & hours_exposed_24_heat >=1,1,0))%>%
           mutate(extreme_wind_ind = ifelse(abs_wind_speed_knots >= wind, 1,0)) %>%
           mutate(hours_exposed_24_wind = runner(extreme_wind_ind, k=9, sum)) %>%
           mutate(wind_driven_outage = ifelse(start_outage == 1 & hours_exposed_24_wind >=1,1,0))%>%
           mutate(extreme_precipitation_ind = ifelse(total_precipitation >= precip, 1,0)) %>%
           mutate(hours_exposed_24_precip = runner(extreme_precipitation_ind, k=9, sum)) %>%
           mutate(precipitation_driven_outage = ifelse(start_outage == 1 & hours_exposed_24_precip >=1,1,0))%>%
           mutate(extreme_snow_ind = ifelse(snowfall_hourly >= snow, 1,0)) %>%
           mutate(hours_exposed_24_snow = runner(extreme_snow_ind, k=9, sum)) %>%
           mutate(snow_driven_outage = ifelse(start_outage == 1 & hours_exposed_24_snow >=1,1,0))%>%
           mutate(extreme_lightning_ind = ifelse(bin_lightning_exposure_hourly >= 1, 1,0)) %>%
           mutate(hours_exposed_24_ltn = runner(extreme_lightning_ind, k=9, sum)) %>%
           mutate(lightning_driven_outage = ifelse(start_outage == 1 & hours_exposed_24_ltn >=1,1,0)))
  
  
  
  
}


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
write.fst(outages_u_defined, "sw_outages_u_defined.fst")
write.fst(outages_r_defined, "sw_outages_r_defined.fst")
write.fst(outages_n_defined, "sw_outages_n_defined.fst")












