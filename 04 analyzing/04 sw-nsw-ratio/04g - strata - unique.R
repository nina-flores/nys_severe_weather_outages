require(dplyr)
require(fst)
require(tidyr)
require(stringr)
require(data.table)


compute_unique <- function(input_file) {
  data <- input_file
  
  # Compute total number of events with each tuple
  sw_variable <- c("cold_driven_outage", "heat_driven_outage", "lightning_driven_outage",
                   "precipitation_driven_outage", "wind_driven_outage")
  
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
           freq = as.numeric(freq),
           combination_clean = gsub("_driven_outage", "", combination))
  
  return(unique_combinations_df)
}




# read data
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")

extreme_u <- read.fst("outages_w_duration_u.fst") %>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))%>%
  select(datetime_eastern,
         dps_id, 
         urn, svi_quartile,
         heat_driven_outage, 
         wind_driven_outage,
         lightning_driven_outage,
         precipitation_driven_outage,
         cold_driven_outage,
         start_outage
  ) %>% filter(start_outage == 1)

extreme_r <- read.fst("outages_w_duration_r.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  select(datetime_eastern,
         dps_id, 
         urn, svi_quartile,
         heat_driven_outage, 
         wind_driven_outage,
         lightning_driven_outage,
         precipitation_driven_outage,
         cold_driven_outage, start_outage)%>% filter(start_outage == 1)

extreme_n <- read.fst("outages_w_duration_n.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  select(datetime_eastern,
         dps_id, 
         urn, svi_quartile,
         heat_driven_outage, 
         wind_driven_outage,
         lightning_driven_outage,
         precipitation_driven_outage,
         cold_driven_outage, start_outage)%>% filter(start_outage == 1)


# merge data
extreme_overall <- rbindlist(list(extreme_u, extreme_r, extreme_n)) %>% 
  mutate_at(vars(heat_driven_outage, 
                 wind_driven_outage,
                 lightning_driven_outage,
                 precipitation_driven_outage,
                 cold_driven_outage), ~replace_na(., 0))


svi1 <- extreme_overall %>%
  filter(svi_quartile == "Q1")

svi2 <- extreme_overall %>%
  filter(svi_quartile == "Q2")

svi3 <- extreme_overall %>%
  filter(svi_quartile == "Q3")

svi4 <- extreme_overall %>%
  filter(svi_quartile == "Q4")

urban <- extreme_overall %>%
  filter(urn == "u")

rural <- extreme_overall %>%
  filter(urn == "r")

nyc <- extreme_overall %>%
  filter(urn == "n")


# Compute frequency for each dataset
freq_combinations_u <- compute_unique(urban)
freq_combinations_r <- compute_unique(rural)
freq_combinations_n <- compute_unique(nyc)
freq_combinations_svi1 <- compute_unique(svi1)
freq_combinations_svi2 <- compute_unique(svi2)
freq_combinations_svi3 <- compute_unique(svi3)
freq_combinations_svi4 <- compute_unique(svi4)
freq_combinations_overall <- compute_unique(extreme_overall)




# Write the results to file
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio")
write.fst(freq_combinations_u, "urban/freq_combinations_u.fst")
write.fst(freq_combinations_r, "rural/freq_combinations_r.fst")
write.fst(freq_combinations_n, "nyc/freq_combinations_n.fst")
write.fst(freq_combinations_svi4 , "svi4/freq_combinations_svi4.fst")
write.fst(freq_combinations_svi3 , "svi3/freq_combinations_svi3.fst")
write.fst(freq_combinations_svi2 , "svi2/freq_combinations_svi2.fst")
write.fst(freq_combinations_svi1 , "svi1/freq_combinations_svi1.fst")
write.fst(freq_combinations_overall , "overall/freq_combinations_overall.fst")



