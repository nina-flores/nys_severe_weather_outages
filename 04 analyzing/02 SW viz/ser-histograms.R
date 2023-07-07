### Quick histograms for SER

### The objective of this script is to create an indicator of outage exposure
### the main exposure, is going to be urban, rural, and NYC specific and the 
### analyses will be conducted separately. 
### Here, for each, I will calculate their 90th percentile of outages
### and then, for each hour, will sum up the number of hours 
### exceeding that threshold in the previous 24 hours. This cumulative
### sum variable will be our main exposure.

require(tidyverse)
require(data.table)
require(dplyr)
require(sf)
require(fst)
require(lubridate)
require(ggpubr)

# read in the data: 
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final")
outages <- read_fst("weather_outages_hourly.fst") %>%
  arrange(datetime_eastern)

########################################
### 1. calculate the outage threshold###
########################################


outages_90th_threshold <- outages %>%
  filter(customers_out > 0) %>% # filters out non outages
  filter(customers > 0) %>% 
  mutate(overall_90 = quantile(prop_out, probs = .90, na.rm = TRUE)) %>%
  group_by(urn) %>%
  mutate(percentile_90th = quantile(prop_out, probs = .90, na.rm = TRUE))%>%
  select(percentile_90th, urn,overall_90) %>%
  unique() %>%
  mutate(percentile_90th = percentile_90th*100)


outages<- outages %>%
  filter(customers_out > 0) %>% # filters out non outages
  filter(customers > 0) %>%
  mutate(percent_out = prop_out *100)


overall <- ggplot(outages, aes(x = percent_out)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = quantile(outages$percent_out, 0.9),
             linetype = "dashed",
             color = "orange") +
  annotate("text", x = quantile(outages$percent_out, 0.9), y = 5,
           label = paste("90th percentile:", round(quantile(outages$percent_out, 0.9), 2)),
           color = "orange",
           hjust = -.05,
           vjust =-10) +
  labs(title = "Histogram of Percent of customers without power",
       x = "Percent of customers without power",
       y = "Count") +
  theme_minimal()+
  scale_y_continuous(labels = comma)

overall


urban <- outages %>%
  filter(urn == "u")

rural <- outages %>%
  filter(urn == "r")

nyc <- outages %>%
  filter(urn == "n")


urban_p <- ggplot(urban, aes(x = percent_out)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = quantile(urban$percent_out, 0.9),
             linetype = "dashed",
             color = "orange") +
  annotate("text", x = quantile(urban$percent_out, 0.9), y = 5,
           label = paste("90th percentile:", round(quantile(urban$percent_out, 0.9), 2)),
           color = "orange",
           hjust = -.05,
           vjust =-10) +
  labs(title = "non-NYC urban",
       x = "Percent of customers without power",
       y = "Count") +
  theme_minimal()+
  scale_y_continuous(labels = comma)

rural_p <- ggplot(rural, aes(x = percent_out)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = quantile(rural$percent_out, 0.9),
             linetype = "dashed",
             color = "orange") +
  annotate("text", x = quantile(rural$percent_out, 0.9), y = 5,
           label = paste("90th percentile:", round(quantile(rural$percent_out, 0.9), 2)),
           color = "orange",
           hjust = -.05,
           vjust =-10) +
  labs(title = "Rural",
       x = "Percent of customers without power",
       y = "Count") +
  theme_minimal()+
  scale_y_continuous(labels = comma)

nyc_p <- ggplot(nyc, aes(x = percent_out)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = quantile(nyc$percent_out, 0.9),
             linetype = "dashed",
             color = "orange") +
  annotate("text", x = quantile(nyc$percent_out, 0.9), y = 5,
           label = paste("90th percentile:", round(quantile(nyc$percent_out, 0.9), 2)),
           color = "orange",
           hjust = -.05,
           vjust =-10) +
  labs(title = "NYC",
       x = "Percent of customers without power",
       y = "Count") +
  theme_minimal()+
  scale_y_continuous(labels = comma)


p <- ggarrange(nyc_p,urban_p,rural_p, nrow = 1)

p




