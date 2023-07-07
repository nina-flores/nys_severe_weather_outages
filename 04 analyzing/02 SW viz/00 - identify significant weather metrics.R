require(tidyverse)
require(data.table)
require(dplyr)
require(sf)
require(fst)
require(lubridate)

# read in the data: 
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final")
sw_outages <- read_fst("weather_outages_hourly.fst")

names(sw_outages)


### get significantly hot temps:

hot <- sw_outages %>%
  filter(month(datetime_eastern) %in% c(5,6,7,8,9)) %>%
  select(temperature)%>%
  summarise(hot = quantile(temperature, probs = .975, na.rm = T))

### get significantly cold temps:
cold <- sw_outages %>%
  filter(month(datetime_eastern) %in% c(10,11,12,1,2,3,4)) %>%
  select(temperature)%>%
  summarise(cold = quantile(temperature, probs = .025, na.rm = T))


### get significantly precip:
precip <- sw_outages %>%
  select(total_precipitation)%>%
  filter(total_precipitation>0)%>%
  summarise(precip = quantile(total_precipitation, probs = .975, na.rm = T))


### get significantly precip:
snow <- sw_outages %>%
  select(snowfall_hourly)%>%
  filter(snowfall_hourly>0)%>%
  summarise(snow = quantile(snowfall_hourly, probs = .975, na.rm = T))


### get significantly windy:
wind <- sw_outages %>%
  select(abs_wind_speed_knots)%>%
  filter(abs_wind_speed_knots>0)%>%
  summarise(wind = quantile(abs_wind_speed_knots, probs = .975, na.rm = T))

sw <- cbind(hot, cold, precip,snow,wind)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/outage indicators")
write.csv(sw, "severe_metrics.csv")



