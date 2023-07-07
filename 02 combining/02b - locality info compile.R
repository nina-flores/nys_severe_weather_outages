### compile urban/rural and svi info to add to outages info
require(fst)
require(tidyverse)
require(dplyr)
require(lubridate)
require(stringr)
require(calendR)
require(viridis)
require(ggExtra)
require(stringr)
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/locality-info")
pods_urn <- read.csv("urban_rural_nyc_pods.csv") %>%
  select(-1)

utility <- read.csv("utility_opdiv_info.csv")  %>%
  select(-1)

svi <- read.csv("svi/pol_svi_2020.csv") %>%
  select(dps_id, svi)%>%
  mutate(dps_id = str_pad(dps_id, width = 5, side = "left", pad = "0"))

locality_info <- pods_urn %>%
  full_join(utility) %>%
  full_join(svi) %>%
  drop_na(urn) %>%
  drop_na(utility)

###
write.csv(locality_info, "locality-info.csv")