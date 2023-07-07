require(dplyr)
require(fst)
require(tidyr)
require(stringr)
require(data.table)

compute_duration_combinations <- function(outages, file_name) {
  outages <- outages %>%
    mutate_at(vars(cold_driven_outage,
                   heat_driven_outage,
                   lightning_driven_outage,
                   precipitation_driven_outage,
                   wind_driven_outage), ~replace_na(., 0))

  
  
  sw_variable <- c("cold_driven_outage", "heat_driven_outage", "lightning_driven_outage",
                   "precipitation_driven_outage", "wind_driven_outage")
  
  # Compute sum of duration for each combination of binary variables
  combinations <- expand.grid(lapply(sw_variable, function(x) c(0, 1)))
  names(combinations) <- sw_variable
  
  duration_combinations <- lapply(seq_len(nrow(combinations)), function(i) {
    # Create a logical vector indicating which rows meet the condition
    condition <- apply(outages[, sw_variable], 1, function(x) all(x == combinations[i,] & !is.na(x)))
    if (any(condition)) {
      # If there are any rows that meet the condition, compute the sum of duration for those rows
      sum_duration <- sum(outages[condition, "start_outage"]) ### changing this to start outage instead of duration
    } else {
      # If there are no rows that meet the condition, set the sum of duration to 0
      sum_duration <- 0
    }
    # Create a named list with the combination and the sum of duration
    list(combination = paste(names(combinations)[which(combinations[i,]==1)], collapse="_"), duration = sum_duration)
  })
  
  names(duration_combinations) <- apply(combinations, 1, function(x) paste(names(x)[x==1], collapse="_"))
  
  # Combine duration_swo and duration_combinations into a single list
  duration_list <-  duration_combinations
  
  # Add names to duration_list
  names(duration_list) <- c(names(duration_combinations))
  
  duration_combinations_df <- do.call(rbind, duration_combinations) %>%
    as.data.frame() %>%
    mutate(combination = as.character(combination),
           duration = as.numeric(duration),
           combination_clean = gsub("_driven_outage", "", combination))
  
  return(duration_combinations_df)
}

# Read in the three datasets
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")

outages_u <- read.fst("outages_w_duration_u.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))
outages_r <- read.fst("outages_w_duration_r.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))
outages_n <- read.fst("outages_w_duration_n.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4")))

outage_duration_overall <- rbind(outages_u, outages_r, outages_n) 

svi1 <- outage_duration_overall %>%
  filter(svi_quartile == "Q1")

svi2 <- outage_duration_overall %>%
  filter(svi_quartile == "Q2")

svi3 <- outage_duration_overall %>%
  filter(svi_quartile == "Q3")

svi4 <- outage_duration_overall %>%
  filter(svi_quartile == "Q4")

# Compute duration combinations for each dataset
duration_combinations_u <- compute_duration_combinations(outages_u, "u")
duration_combinations_r <- compute_duration_combinations(outages_r, "r")
duration_combinations_n <- compute_duration_combinations(outages_n, "n")
duration_combinations_svi1 <- compute_duration_combinations(svi1, "svi1")
duration_combinations_svi2 <- compute_duration_combinations(svi2, "svi2")
duration_combinations_svi3 <- compute_duration_combinations(svi3, "svi3")
duration_combinations_svi4 <- compute_duration_combinations(svi4, "svi4")
duration_combinations_overall <- compute_duration_combinations(outage_duration_overall, "overall")




# Write the results to file
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio")
write.fst(duration_combinations_overall, "overall/outage-durations_overall.fst")
write.fst(duration_combinations_u, "urban/outage-durations_u.fst")
write.fst(duration_combinations_r, "rural/outage-durations_r.fst")
write.fst(duration_combinations_n, "nyc/outage-durations_n.fst")
write.fst(duration_combinations_svi4 , "svi4/outage-durations_svi4.fst")
write.fst(duration_combinations_svi3 , "svi3/outage-durations_svi3.fst")
write.fst(duration_combinations_svi2 , "svi2/outage-durations_svi2.fst")
write.fst(duration_combinations_svi1 , "svi1/outage-durations_svi1.fst")


