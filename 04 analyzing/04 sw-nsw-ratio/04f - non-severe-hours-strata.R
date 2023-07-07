require(dplyr)
require(fst)
require(tidyr)
require(stringr)
require(data.table)

calculate_non_severe_outage_hours <- function(outage_duration) {
  non_severe_outage_hours <- outage_duration %>%
    filter(cold_driven_outage +
             heat_driven_outage +
             lightning_driven_outage +
             precipitation_driven_outage +
             wind_driven_outage == 0) %>%
    mutate(ns_outage_hours = sum(start_outage)) %>% ### changing from duration to start_outage
    slice(1) %>%
    select(ns_outage_hours)
  
  return(non_severe_outage_hours)
}

calculate_non_severe_hours <- function(outage_data) {
  non_severe_hours <- outage_data %>%
    filter(extreme_cold_ind +
             extreme_heat_ind+
             extreme_wind_ind +
             extreme_precipitation_ind +
             extreme_lightning_ind == 0) %>%
    mutate(ns_hours = n()) %>%
    slice(1) %>%
    select(ns_hours)
  
  return(non_severe_hours)
}

# read data
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_u <- read.fst("outages_w_duration_u.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))%>% 
  mutate_at(vars(cold_driven_outage,
                 heat_driven_outage,
                 lightning_driven_outage,
                 precipitation_driven_outage,
                 wind_driven_outage), ~replace_na(., 0))
outages_r <- read.fst("outages_w_duration_r.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))%>% 
  mutate_at(vars(cold_driven_outage,
                 heat_driven_outage,
                 lightning_driven_outage,
                 precipitation_driven_outage,
                 wind_driven_outage), ~replace_na(., 0))
outages_n <- read.fst("outages_w_duration_n.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))%>% 
  mutate_at(vars(cold_driven_outage,
                 heat_driven_outage,
                 lightning_driven_outage,
                 precipitation_driven_outage,
                 wind_driven_outage), ~replace_na(., 0))

outage_duration_overall <- rbind(outages_u, outages_r, outages_n) %>% 
  mutate_at(vars(cold_driven_outage,
                 heat_driven_outage,
                 lightning_driven_outage,
                 precipitation_driven_outage,
                 wind_driven_outage), ~replace_na(., 0))


svi1 <- outage_duration_overall %>%
  filter(svi_quartile == "Q1")

svi2 <- outage_duration_overall %>%
  filter(svi_quartile == "Q2")

svi3 <- outage_duration_overall %>%
  filter(svi_quartile == "Q3")

svi4 <- outage_duration_overall %>%
  filter(svi_quartile == "Q4")




# Compute duration combinations for each dataset
non_severe_outage_hours_u <- calculate_non_severe_outage_hours(outages_u)
non_severe_outage_hours_r <- calculate_non_severe_outage_hours(outages_r)
non_severe_outage_hours_n <- calculate_non_severe_outage_hours(outages_n)
non_severe_outage_hours_svi1 <- calculate_non_severe_outage_hours(svi1)
non_severe_outage_hours_svi2 <- calculate_non_severe_outage_hours(svi2)
non_severe_outage_hours_svi3 <- calculate_non_severe_outage_hours(svi3)
non_severe_outage_hours_svi4 <- calculate_non_severe_outage_hours(svi4)
non_severe_outage_hours_overall <- calculate_non_severe_outage_hours(outage_duration_overall)




# Write the results to file
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio")
write.fst(non_severe_outage_hours_u, "urban/non_severe_outage_hours_u.fst")
write.fst(non_severe_outage_hours_r, "rural/non_severe_outage_hours_r.fst")
write.fst(non_severe_outage_hours_n, "nyc/non_severe_outage_hours_n.fst")
write.fst(non_severe_outage_hours_svi4 , "svi4/non_severe_outage_hours_svi4.fst")
write.fst(non_severe_outage_hours_svi3 , "svi3/non_severe_outage_hours_svi3.fst")
write.fst(non_severe_outage_hours_svi2 , "svi2/non_severe_outage_hours_svi2.fst")
write.fst(non_severe_outage_hours_svi1 , "svi1/non_severe_outage_hours_svi1.fst")
write.fst(non_severe_outage_hours_overall , "overall/non_severe_outage_hours_overall.fst")






setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")


outages_uu <- read.fst("sw_outages_u_defined.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))%>% 
  mutate_at(vars(extreme_cold_ind ,
                 extreme_heat_ind,
                 extreme_wind_ind ,
                 extreme_precipitation_ind ,
                 extreme_lightning_ind), ~replace_na(., 0))
outages_rr <- read.fst("sw_outages_r_defined.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))%>% 
  mutate_at(vars(extreme_cold_ind ,
                 extreme_heat_ind,
                 extreme_wind_ind ,
                 extreme_precipitation_ind ,
                 extreme_lightning_ind), ~replace_na(., 0))
outages_nn <- read.fst("sw_outages_n_defined.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))%>% 
  mutate_at(vars(extreme_cold_ind ,
                 extreme_heat_ind,
                 extreme_wind_ind ,
                 extreme_precipitation_ind ,
                 extreme_lightning_ind), ~replace_na(., 0))

outage_overall <- rbind(outages_uu, outages_rr, outages_nn)%>% 
  mutate_at(vars(extreme_cold_ind ,
                   extreme_heat_ind,
                   extreme_wind_ind ,
                   extreme_precipitation_ind ,
                   extreme_lightning_ind), ~replace_na(., 0))




svi11 <- outage_overall %>%
  filter(svi_quartile == "Q1")

svi22 <- outage_overall %>%
  filter(svi_quartile == "Q2")

svi33 <- outage_overall %>%
  filter(svi_quartile == "Q3")

svi44 <- outage_overall %>%
  filter(svi_quartile == "Q4")

# Compute duration combinations for each dataset
non_severe_hours_u <- calculate_non_severe_hours(outages_uu)
non_severe_hours_r <- calculate_non_severe_hours(outages_rr)
non_severe_hours_n <- calculate_non_severe_hours(outages_nn)
non_severe_hours_svi1 <- calculate_non_severe_hours(svi11)
non_severe_hours_svi2 <- calculate_non_severe_hours(svi22)
non_severe_hours_svi3 <- calculate_non_severe_hours(svi33)
non_severe_hours_svi4 <- calculate_non_severe_hours(svi44)
non_severe_hours_overall <- calculate_non_severe_hours(outage_overall)




# Write the results to file
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio")
write.fst(non_severe_hours_u, "urban/non_severe_hours_u.fst")
write.fst(non_severe_hours_r, "rural/non_severe_hours_r.fst")
write.fst(non_severe_hours_n, "nyc/non_severe_hours_n.fst")
write.fst(non_severe_hours_svi4 , "svi4/non_severe_hours_svi4.fst")
write.fst(non_severe_hours_svi3 , "svi3/non_severe_hours_svi3.fst")
write.fst(non_severe_hours_svi2 , "svi2/non_severe_hours_svi2.fst")
write.fst(non_severe_hours_svi1 , "svi1/non_severe_hours_svi1.fst")
write.fst(non_severe_hours_overall , "overall/non_severe_hours_overall.fst")




