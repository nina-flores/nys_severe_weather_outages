### add hourly outage and hourly met data info
require(fst)
require(tidyverse)
require(dplyr)
require(lubridate)
require(stringr)
require(calendR)
require(viridis)
require(ggExtra)


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/0 combined-files")
dta_weather <- read_fst("hourly_severe_events.fst")


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final")
dta_outages <- read.fst("outages-and-locality-info.fst", as.data.table = TRUE)

names(dta_outages)

dps <- unique(dta_outages$dps_id)

dta_weather_filtered <- dta_weather %>%
  filter(PRIME_DPS_ %in% dps) %>%
  dplyr::rename(dps_id = PRIME_DPS_) %>%
  mutate(datetime_utc = as.POSIXct(date_hour, tz = "UTC"))%>%
  mutate(datetime_eastern = with_tz(datetime_utc, "America/New_York")) # the outages data is eastern - converting met data for proper join.

rm(dta_weather)

dta_outages <-dta_outages%>% mutate(datetime_eastern = datetime)

weather_outages <- full_join(dta_weather_filtered, dta_outages) 


rm(dta_outages)
### filter to only the variables necessary
weather_outages_filt <- weather_outages %>%
  filter(year(datetime_eastern) %in% c(2017,2018,2019,2020))%>%
  select(datetime_eastern,
         NAME, 
         dps_id,
         customers, 
         customers_out,
         prop_out, 
         utility,
         op_div,
         urn, 
         svi,
         temperature,
         abs_wind_speed_knots,
         total_precipitation,
         rh,
         bin_lightning_exposure_hourly,
         num_hourly_lightning_exposure,
         snowfall_hourly) 

rm(weather_outages)

write.fst(weather_outages_filt, "weather_outages_hourly.fst")
