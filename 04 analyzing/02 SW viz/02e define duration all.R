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
    mutate(duration = sum(i_gt_90pct),
           n = n(),
           prop = mean(prop_out)*100) %>%
    ungroup()
  assign(paste0("outages_", i, "_duration"), df)
  
}
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
### write out all outages with duration
write.fst(outages_u_duration, "outages_u_duration.fst")
write.fst(outages_r_duration, "outages_r_duration.fst")
write.fst(outages_n_duration, "outages_n_duration.fst")


