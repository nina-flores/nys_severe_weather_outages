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



setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_u_defined <- read.fst("sw_outages_u_defined.fst") %>%
  select(dps_id, 
         cold_driven_outage,
         heat_driven_outage,wind_driven_outage,
         precipitation_driven_outage,
         snow_driven_outage,
         wind_driven_outage,
         lightning_driven_outage,
          svi,
         utility)
outages_r_defined <- read.fst("sw_outages_r_defined.fst")%>%
  select(dps_id, 
         cold_driven_outage,
         heat_driven_outage,wind_driven_outage,
         precipitation_driven_outage,
         snow_driven_outage,
         wind_driven_outage,
         lightning_driven_outage,
          svi,
         utility)
outages_n_defined <- read.fst("sw_outages_n_defined.fst")%>%
  select(dps_id, 
         cold_driven_outage,
         heat_driven_outage,wind_driven_outage,
         wind_driven_outage,
         precipitation_driven_outage,
         snow_driven_outage,
         lightning_driven_outage,
          svi,
         utility)



run <- list("u", "r", "n")

for(i in run){

assign(paste0("sw_outages_",i,"_any"), get(paste0("outages_",i,"_defined"))%>%
         group_by(dps_id) %>%
         mutate(sum_cold_outages = sum(cold_driven_outage, na.rm = T),
         sum_cold_outages_bin = ifelse(sum_cold_outages >= 1, 1,0),
         sum_heat_outages = sum(heat_driven_outage, na.rm = T),
         sum_heat_outages_bin = ifelse(sum_heat_outages >= 1, 1,0),
         sum_wind_outages = sum(wind_driven_outage, na.rm = T),
         sum_wind_outages_bin = ifelse(sum_wind_outages >= 1, 1,0),
         sum_precip_outages = sum(precipitation_driven_outage, na.rm = T),
         sum_precip_outages_bin = ifelse(sum_precip_outages >= 1, 1,0),
         sum_snow_outages = sum(snow_driven_outage, na.rm = T),
         sum_snow_outages_bin = ifelse(sum_snow_outages >= 1, 1,0),
         sum_ltn_outages = sum(lightning_driven_outage, na.rm = T),
         sum_ltn_outages_bin = ifelse(sum_ltn_outages >= 1, 1,0)) %>%
    select(dps_id, 
            svi,
           sum_cold_outages_bin,
           sum_heat_outages_bin,
           sum_wind_outages_bin,
           sum_precip_outages_bin,
           sum_snow_outages_bin,
           sum_ltn_outages_bin,
           sum_cold_outages,
           sum_heat_outages,
           sum_wind_outages,
           sum_precip_outages,
           sum_snow_outages,
           sum_ltn_outages,
           utility)%>%
    filter(row_number()==1))

}




setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
write.fst(sw_outages_u_any, "sw_outages_u_any.fst")
write.fst(sw_outages_r_any, "sw_outages_r_any.fst")
write.fst(sw_outages_n_any, "sw_outages_n_any.fst")

