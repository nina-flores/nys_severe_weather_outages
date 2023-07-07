

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




overall <- ggplot(sw_outages, aes(x = abs_wind_speed_knots)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = quantile(sw_outages$abs_wind_speed_knots, 0.975, na.rm = T),
             linetype = "dashed",
             color = "orange") +
  annotate("text", x = quantile(sw_outages$abs_wind_speed_knots, 0.975, na.rm = T), y = 5,
           label = paste("97.5th percentile:", round(quantile(sw_outages$abs_wind_speed_knots, 0.975, na.rm = T), 2)),
           color = "orange",
           hjust = -.05,
           vjust =-10) +
  labs(title = "Histogram of hourly windspeeds",
       x = "Windspeed (knots)",
       y = "Count") +
  theme_minimal()+
  scale_y_continuous(labels = comma)

overall



