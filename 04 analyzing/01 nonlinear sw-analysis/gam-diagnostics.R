#### check for spatial autocorrelation


library(dlnm)
library(mgcv)
require(splines)
require(glm)
require(fst)
require(dplyr)
require(lubridate)
require(tidyr)
require(sf)
require(spdep)
require(ggplot2)
require(gratia)
#install.packages('DHARMa', dependencies=TRUE, type="source")
require(DHARMa)


### read in overall data
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final")

set.seed(4)

overall_dta <- read_fst("weather_outages_hourly.fst")%>%
  dplyr::select(dps_id,temperature, total_precipitation, snowfall_hourly, abs_wind_speed_knots,customers, customers_out, utility, op_div, urn,
                datetime_eastern, prop_out) %>%
  mutate(hour = hour(datetime_eastern),
         year = year(datetime_eastern),
         dt = as.Date(datetime_eastern)) %>%
  mutate(snowfall_hourly = if_else(snowfall_hourly<0, 0, snowfall_hourly))%>%
  sample_frac(.01)




### read in locality data
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/e_locality")

locality <- st_read("e_locality.shp") %>%
  mutate(dps_id = PRIME_DPS_)

locality <- locality %>% st_transform("EPSG:4326") %>%
  st_make_valid()

locality$cents <- st_centroid(locality$geometry)


coordinates <- st_coordinates(locality$cents)
locality <- locality %>%
  mutate(
    lat = coordinates[, 2],
    lon = coordinates[, 1]) %>%
  select(dps_id,lat, lon)

### spatial join data 
overall_dta <- inner_join(overall_dta, locality)



### all 2, 5 ######################################
###################################################


### run for nyc###
dta_n <- overall_dta %>%
  filter(urn == "n") %>% 
  drop_na(customers_out, customers,total_precipitation, dt,utility)



# create crossbasis with natural spline
cb <- crossbasis(
  dta_n$total_precipitation,       
  lag = 24,    
  argvar = list(fun="ns", df = 2),  # use a natural spline for the exposure-response curve
  arglag = list(fun="ns", df = 5)  # use a natural spline for the lag-response curve
)

# run gam with crossbasis and automatic selection of df
mod.ns_tw <- gam(round(customers_out) ~ cb + factor(utility) + ns(dt, df = 4 * 6)+ s(lat,lon, k = 4),
                 family = tw(),
                 offset = log(customers),
                 data = dta_n,
                 method = "REML")


simulationOutput <- simulateResiduals(fittedModel = mod.ns_tw)
plotResiduals(simulationOutput, smoothScatter = F)
plotQQunif(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)



mod.ns_qpoi <- gam(round(customers_out) ~ cb + factor(utility) + ns(dt, df = 4 * 6)+ s(lat,lon, k = 5),
                   family =  "poisson",
                   offset = log(customers),
                   data = dta_n)



simulationOutput <- simulateResiduals(fittedModel = mod.ns_qpoi)
plotResiduals(simulationOutput, smoothScatter = F)
plotQQunif(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)




mod.ns_nb <- gam(round(customers_out) ~ cb + factor(utility) + ns(dt, df = 4 * 6)+ s(lat,lon, k = 4),
                 family =  nb(theta = NULL, link = "log"),
                 offset = log(customers),
                 data = dta_n)


simulationOutput <- simulateResiduals(fittedModel = mod.ns_nb)
plotResiduals(simulationOutput, smoothScatter = F)
plotQQunif(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)



mod.ns_zip <-gam(round(customers_out) ~ cb + factor(utility) + ns(dt, df = 4 * 6)+ s(lat,lon, k = 4),
                 family=ziP(),
                 offset = log(customers),
                 data = dta_n)

simulationOutput <- simulateResiduals(fittedModel = mod.ns_zip)
plotResiduals(simulationOutput, smoothScatter = F)
plotQQunif(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

