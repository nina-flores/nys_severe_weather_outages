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
outages_r <- read_fst("rural_outages.fst")
outages_u <- read_fst("urban_outages.fst")
outages_n <- read_fst("nyc_outages.fst")



run <- list("n", "u", "r")

for(i in run){
  
  df <- get(paste0("outages_", i))
  df <- df %>%
    group_by(ind_90pct) %>%
    mutate(duration = sum(i_gt_90pct)) %>%
    select(duration, dps_id, datetime_eastern, ind_90pct) %>%
    ungroup()
  assign(paste0("outages_", i, "_duration"), df)
  
         }


# join outage duration information to sw outage information
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_u_defined <- read.fst("sw_outages_u_defined.fst")
outages_r_defined <- read.fst("sw_outages_r_defined.fst")
outages_n_defined <- read.fst("sw_outages_n_defined.fst")

outages_u_dur <- left_join(outages_u_defined,outages_u_duration ) %>%
  filter(start_outage == 1)
outages_r_dur <- left_join(outages_r_defined,outages_r_duration )%>%
  filter(start_outage == 1)
outages_n_dur <- left_join(outages_n_defined,outages_n_duration )%>%
  filter(start_outage == 1)



outages_n_dur_long <- outages_n_dur %>%
  pivot_longer(ends_with("driven_outage")) %>%
  filter(value == 1)


outages_u_dur_long <- outages_u_dur %>%
  pivot_longer(ends_with("driven_outage")) %>%
  filter(value == 1)


outages_r_dur_long <- outages_r_dur %>%
  pivot_longer(ends_with("driven_outage")) %>%
  filter(value == 1)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
write.fst(outages_n_dur_long, "outages_n_dur_long.fst")
write.fst(outages_u_dur_long, "outages_u_dur_long.fst")
write.fst(outages_r_dur_long, "outages_r_dur_long.fst")

