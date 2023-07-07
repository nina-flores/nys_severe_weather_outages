require(dplyr)
require(stringr)
require(tidyverse)
require(weathermetrics)
require(lubridate)
require(fst)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/era-snow")
snow <- read_fst("snow_utility.fst")



### splitting the system index variable:


snow_sep <-snow %>% 
  tidyr::separate("system:index", into = c("date", "hour"), sep = "T")%>%
  tidyr::separate("hour", into = c("hour", NA), sep = "_") %>%
  mutate(date = ymd(date)) %>%
  mutate(dt = date) %>%
  unite(date_hour, c("date", "hour"), sep = " ")

write.fst(snow_sep, "snow_utility_clean.fst")



#make daily indicator of snow - daily max
snow_daily <-snow_sep %>% 
  group_by(dt, PRIME_DPS_,NAME)%>%
  mutate(max_snowfall_daily = max(snowfall_hourly))%>%
  mutate(sum_snowfall_daily = sum(snowfall_hourly))

sd <- snow_daily %>%
  select(dt, PRIME_DPS_,NAME, max_snowfall_daily, sum_snowfall_daily)%>%
  unique()

write.fst(snow_daily, "snow_daily.fst")
write.fst(sd, "snow_daily_unique.fst")



