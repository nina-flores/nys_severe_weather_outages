#the objective of this script is to compile all hourly severe weather info
#into one document

require(fst)
require(tidyverse)
require(dplyr)
require(lubridate)
require(stringr)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/")


#continuous measures (hourly):
continuous <- read.fst("NLDAS_utility/NLDAS_utility_variables.fst")
continuous$date_hour<- gsub('.{2}$', '', continuous$date_hour) 

continuous<- continuous %>%
  dplyr::mutate(mins = ":00") %>%
  dplyr::mutate(secs = ":00") %>%
  unite("date_hour", c("date_hour", "mins", "secs"), sep = "")
names(continuous) #date_hour

#snow (hourly):
snow <- read.fst("era-snow/snow_utility_clean.fst")%>%
  dplyr::mutate(mins = ":00") %>%
  dplyr::mutate(secs = ":00") %>%
  unite("date_hour", c("date_hour", "mins", "secs"), sep = "")


#lightning hourly (hourly):
lightning_hr <- read.fst("lightning-points/lightning-hourly-utility.fst")%>%
  dplyr::mutate(date_hour = as.character(time_utc_hourly))
names(lightning_hr) #time_utc_hourly


###### get monthly count:
lightning_monthly <- lightning_hr %>%
  dplyr::mutate(mn = lubridate::month(time_utc_hourly),
               yr = lubridate::year(time_utc_hourly),) %>%
  group_by(mn, yr) %>%
  mutate(b = sum(num_hourly_lightning_exposure)) %>%
  slice(1)


ap_aug <- lightning_monthly %>%
  filter(mn %in% c(4:8)) %>%
  mutate(a = sum(b))
  
sum(ap_aug$b)

# all of these should be in utc

#combine all hourly measures:
hourly_metrics <- continuous %>%
  full_join(lightning_hr) %>%
  full_join(snow)


#write out
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/0 combined-files")

write_fst(hourly_metrics, "hourly_severe_events.fst")


