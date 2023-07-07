require(dplyr)
require(fst)
require(tidyr)
require(stringr)
require(data.table)


# read data
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")

extreme_u <- read.fst("sw_outages_u_defined.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  select(datetime_eastern,
         dps_id, 
         urn, svi_quartile,
         extreme_heat_ind, 
         extreme_wind_ind,
         extreme_lightning_ind,
         extreme_precipitation_ind,
         extreme_cold_ind)%>% 
  mutate_at(vars(extreme_heat_ind, 
                 extreme_wind_ind,
                 extreme_lightning_ind,
                 extreme_precipitation_ind,
                 extreme_cold_ind), ~replace_na(., 0))

extreme_r <- read.fst("sw_outages_r_defined.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  select(datetime_eastern,
         dps_id, 
         urn,  svi_quartile,
         extreme_heat_ind, 
         extreme_wind_ind,
         extreme_lightning_ind,
         extreme_precipitation_ind,
         extreme_cold_ind)%>% 
  mutate_at(vars(extreme_heat_ind, 
                 extreme_wind_ind,
                 extreme_lightning_ind,
                 extreme_precipitation_ind,
                 extreme_cold_ind), ~replace_na(., 0))

extreme_n <- read.fst("sw_outages_n_defined.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  select(datetime_eastern,
         dps_id, 
         urn, svi_quartile,
         extreme_heat_ind, 
         extreme_wind_ind,
         extreme_lightning_ind,
         extreme_precipitation_ind,
         extreme_cold_ind)%>% 
  mutate_at(vars(extreme_heat_ind, 
                 extreme_wind_ind,
                 extreme_lightning_ind,
                 extreme_precipitation_ind,
                 extreme_cold_ind), ~replace_na(., 0))

# merge data
extreme_overall <- rbindlist(list(extreme_u, extreme_r, extreme_n)) 

svi1 <- extreme_overall %>%
  filter(svi_quartile == "Q1")

svi2 <- extreme_overall %>%
  filter(svi_quartile == "Q2")

svi3 <- extreme_overall %>%
  filter(svi_quartile == "Q3")

svi4 <- extreme_overall %>%
  filter(svi_quartile == "Q4")




compute_events <- function(input_file) {
  data <- input_file
  
  # Compute total number of events with each tuple
  sw_variable <- c("extreme_cold_ind", "extreme_heat_ind", "extreme_lightning_ind",
                   "extreme_precipitation_ind", "extreme_wind_ind")
  
  # Compute sum of duration for each combination of binary variables using data.table
  dt_data <- as.data.table(data)
  dt_combs <- CJ(0:1, 0:1, 0:1, 0:1, 0:1)
  setnames(dt_combs, sw_variable)
  
  unique_combinations <- dt_combs[, {
    condition <- Reduce(`&`, Map(`==`, dt_data[, sw_variable, with = FALSE], .SD))
    if (any(condition)) {
      sum_storms <- sum(dt_data[condition, ..sw_variable])
      freq <- sum(condition)
    } else {
      sum_storms <- 0
      freq <- 0
    }
    list(combination = paste(names(.SD)[.SD == 1], collapse = "_"), freq = as.integer(freq), sum_storms = sum_storms)
  }, by = 1:nrow(dt_combs)]
  
  unique_combinations_df <- as.data.frame(unique_combinations) %>%
    mutate(combination = as.character(combination),
           sw_events = as.numeric(freq),
           combination_clean = gsub("extreme_", "", combination),
           combination_clean = gsub("_ind", "", combination_clean),) %>%
    select(combination_clean, sw_events)
  
  return(unique_combinations_df)
}






# Call compute_duration function for each dataset
sw_duration_u <- compute_events(extreme_u)
sw_duration_r <- compute_events(extreme_r)
sw_duration_n <- compute_events(extreme_n)
sw_duration_svi1 <- compute_events(svi1)
sw_duration_svi2 <- compute_events(svi2)
sw_duration_svi3 <- compute_events(svi3)
sw_duration_svi4 <- compute_events(svi4)
sw_duration_overall <- compute_events(extreme_overall)





# Write the results to file
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio")
write.fst(sw_duration_u, "urban/storm-event-durations-u.fst")
write.fst(sw_duration_r, "rural/storm-event-durations-r.fst")
write.fst(sw_duration_n, "nyc/storm-event-durations-n.fst")
write.fst(sw_duration_svi4 , "svi4/storm-event-durations-svi4.fst")
write.fst(sw_duration_svi3 , "svi3/storm-event-durations-svi3.fst")
write.fst(sw_duration_svi2 , "svi2/storm-event-durations-svi2.fst")
write.fst(sw_duration_svi1 , "svi1/storm-event-durations-svi1.fst")
write.fst(sw_duration_overall , "overall/storm-event-durations-overall.fst")

