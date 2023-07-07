require(dplyr)
require(stringr)
require(tidyverse)
require(weathermetrics)
require(lubridate)
require(fst)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/NLDAS_utility")

nldas <- read_fst("NLDAS_utility_1400.fst")



### splitting the system index variable:


nldas_sep <-nldas %>% 
  tidyr::separate("system:index", into = c("date", "hour", NA), sep = "_")%>%
  dplyr::mutate(date = str_sub(date, 2, -1)) # drop the leading A

nldas_un <- nldas_sep%>%
  mutate(date = ymd(date)) %>%
  unite(date_hour, c("date", "hour"), sep = " ")





### create other variables of interest:
nldas_var <- nldas_un %>%
  mutate(abs_wind_speed = sqrt((wind_u)^2 + (wind_v)^2), ## this should be m/s
         abs_wind_speed_knots = speed_to_knots(abs_wind_speed, unit = "mps", round = 3))

##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 The computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

nldas_var <- nldas_var %>%
  mutate(pres_mb = .01 * pressure,
         rh = qair2rh(specific_humidity, temperature, pres_mb),
         heat_index_f = heat.index(t = temperature, rh = rh, temperature.metric = "celsius", round = 3, output.metric = "farenheit"),
         heat_index_c = heat.index(t = temperature, rh = rh, temperature.metric = "celsius", round = 3, output.metric = "celsius"))


write_fst(nldas_var, "NLDAS_utility_variables.fst")



