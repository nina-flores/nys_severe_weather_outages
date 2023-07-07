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
    condition <- apply(outages[, sw_variable], 1, function(x) all(x == combinations[i,] ))
    if (any(condition)) {
      # If there are any rows that meet the condition, compute the mean and sd duration for those rows
      mean_duration <- mean(outages[condition, "duration"])
      sd_duration <- sd(outages[condition, "duration"])
      freq = sum(condition)      
    } else {
      # If there are no rows that meet the condition, set the sum of duration to 0
      mean_duration <- 0
      sd_duration <- 0
      freq <- 0
      
      
    }
    # Create a named list with the combination and the sum of duration
    list(combination = paste(names(combinations)[which(combinations[i,]==1)], collapse="_"), 
         mean_duration = mean_duration, sd_duration = sd_duration, freq = freq)
  })
  
  duration_combinations_df <- do.call(rbind, duration_combinations) %>%
    as.data.frame() %>%
    mutate(combination = as.character(combination),
           mean_duration = as.numeric(mean_duration),
           sd_duration = as.numeric(sd_duration),
           freq = as.numeric(freq),
           strata = file_name,
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


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/table-duration-freq-prop")
# Write the results to file
write.fst(duration_combinations_u , "durations-u.fst")
write.fst(duration_combinations_r , "durations-r.fst")
write.fst(duration_combinations_n , "durations-n.fst")
write.fst(duration_combinations_svi4 , "durations-svi4.fst")
write.fst(duration_combinations_svi3 , "durations-svi3.fst")
write.fst(duration_combinations_svi2 , "durations-svi2.fst")
write.fst(duration_combinations_svi1 , "durations-svi1.fst")
write.fst(duration_combinations_overall , "durations-overall.fst")


