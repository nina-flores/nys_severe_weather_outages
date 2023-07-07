require(dplyr)
require(fst)
require(tidyr)
require(stringr)
require(data.table)


# define a function to calculate co-occurrence ratios
cooccurrence <- function(path, subdir) {
  # set the working directory to the path and subdirectory
  setwd(paste(path, subdir, sep = ""))
  
  # read the input data files
  non_severe_hours <- read.fst(list.files(pattern = "non_severe_hours"))
  sw_duration_combinations_df <- read.fst(list.files(pattern = "storm-event-duration")) %>%
    mutate(combination = combination_clean)
  duration_combinations_df <- read.fst(list.files(pattern = "outage-durations")) %>%
    mutate(combination = combination_clean)
  non_severe_outage_hours <- read.fst(list.files(pattern = "non_severe_outage_hours"))
  freq_combinations_df <- read.fst(list.files(pattern = "freq_combinations")) %>%
    mutate(combination = combination_clean)
  
  
  
  
  # calculate the co-occurrence ratios
  data <- full_join(sw_duration_combinations_df, duration_combinations_df ) %>%
    full_join(freq_combinations_df) %>%
    mutate(ns_hours  = non_severe_hours$ns_hours,
           ns_outage_hours = non_severe_outage_hours$ns_outage_hours,
           cooccurrence = ((duration/sw_events) / (ns_outage_hours/ns_hours)),
           strata = subdir) %>%
    filter(!is.na(cooccurrence)) %>%
    filter(duration > 0)
  
  # return the results
  return(data)
}

# set the path to the directory containing the subdirectories
path <- "~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio/"


# define the subdirectories to process
subdirs <- c("svi1/", "svi2/", "svi3/", "svi4/", "urban/", "rural/", "nyc/", "overall/")

# process each subdirectory and store the results in a list
results <- lapply(subdirs, cooccurrence, path = path)

# combine the results into a single data frame
combined_results <- bind_rows(results) %>%
  mutate(combination = gsub("_", " + ", combination)) %>%
  mutate(strata = gsub("/", "", strata)) 


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio/overall")
t <- read.fst("freq_combinations_overall.fst")
t <- read.fst("storm-event-durations-overall.fst")
t <- read.fst("non_severe_hours_overall.fst")





###-----------------------------------------------------------------------------
### make a table
###-----------------------------------------------------------------------------
### first urban/rural/nyc

data_pretty_urbanicity <- combined_results %>%
  filter(strata == "overall"| strata == "urban" | strata == "rural" | strata == "nyc") %>%
  select(combination, cooccurrence,strata, freq) %>%
  mutate(type = if_else(str_detect(combination, "\\+"), "multiple", "single")) %>% 
  arrange(desc(type), desc(cooccurrence))%>%
  select(type, combination,  cooccurrence,strata, freq) %>%
  rename(`Frequency` = freq,
         `Ratio` = cooccurrence,
         `Combination driving outage` = combination,
         `Type` = type) %>%
  pivot_wider(names_from = strata, values_from = c(`Frequency`,`Ratio`)) %>%
  select(Type, `Combination driving outage`, Frequency_overall, Ratio_overall,
         Frequency_nyc, Ratio_nyc, Frequency_urban, Ratio_urban, Frequency_rural,
         Ratio_rural)

names(data_pretty_urbanicity)

### now svi

data_pretty_svi<- combined_results %>%
  filter(strata == "svi1" | strata == "svi2" | strata == "svi3" | strata == "svi4" ) %>%
  select(combination, cooccurrence,strata, freq) %>%
  mutate(type = if_else(str_detect(combination, "\\+"), "multiple", "single")) %>% 
  arrange(desc(type), desc(cooccurrence))%>%
  select(type, combination,  cooccurrence,strata, freq) %>%
  rename(`Frequency` = freq,
         `Ratio` = cooccurrence,
         `Combination driving outage` = combination,
         `Type` = type
  ) %>%
  pivot_wider(names_from = strata, values_from = c(`Frequency`,`Ratio`))  %>%
  select(Type, `Combination driving outage`, Frequency_svi1, Ratio_svi1,
         Frequency_svi2, Ratio_svi2, Frequency_svi3, Ratio_svi3, Frequency_svi4,
         Ratio_svi4)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/sw-nsw-ratio")
write.csv(data_pretty_svi, "ratio-svi.csv")
write.csv(data_pretty_urbanicity, "ratio-urbanicity.csv")

