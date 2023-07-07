require(dplyr)
require(fst)
require(tidyr)
require(stringr)
require(data.table)
library(officer)
library(officedown)
library(flextable)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/table-duration-freq-prop")
duration_dta <- read.fst(list.files(pattern = "durations"))


# Set the directory path where the .fst files are located
directory <- "~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/table-duration-freq-prop"

# List all files in the directory
files_duration <- list.files(directory, pattern = "duration", full.names = TRUE)

# Initialize an empty data frame
duration_data <- data.frame()

# Read and rbind the contents of each file
for (file in files_duration) {
  data <- read.fst(file)
  duration_data <- rbind(duration_data, data)
}

# Print the combined data frame
print(duration_data)





# List all files in the directory
files_prop_out <- list.files(directory, pattern = "prop_out", full.names = TRUE)

# Initialize an empty data frame
prop_out_data <- data.frame()

# Read and rbind the contents of each file
for (file in files_prop_out) {
  data <- read.fst(file)
  prop_out_data <- rbind(prop_out_data, data)
}

data <- full_join(duration_data, prop_out_data)


################################################################################
### tables
################################################################################
data_pretty_urbanicity <- data %>%
  mutate(combination = gsub("_", " + ", combination_clean)) %>%
  filter( strata == "svi1" | strata == "svi2" | strata == "svi3"| strata == "svi4") %>%
  select(combination, strata, freq, mean_duration, sd_duration,mean_prop_out, sd_prop_out) %>%
  mutate(type = if_else(str_detect(combination, "\\+"), "multiple", "single")) %>% 
  arrange(desc(type), desc(freq))%>%
  mutate(duration = paste0(round(mean_duration,2)," (",round(sd_duration,2),")"),
         prop_out = paste0(round(mean_prop_out,2)," (",round(sd_prop_out,2),")")) %>%
  mutate(freq = round(freq,2)) %>%
  rename(Frequency = freq,
         Duration = duration,
         Combination = combination,
         Type = type,
         Proportion = prop_out) %>%
  select(Frequency ,
         Duration ,
         Combination ,
         Type, strata,Proportion) %>%
  pivot_wider(names_from = strata, values_from = c(Frequency, Duration,Proportion)) %>%
  filter(Frequency_svi1 >0) %>%
  mutate(Combination = if_else(Combination == "", "no severe weather", Combination))


combined_data_with_total <- bind_rows(data_pretty_urbanicity, summarise_if(data_pretty_urbanicity, is.numeric, sum))%>%
  mutate(Combination = if_else(is.na(Combination)== TRUE, "Total", Combination)) %>%
  mutate(across(where(is.numeric), ~ sprintf("%s (%.2f)", ., (. / last(.)) * 100))) %>%
  select(Type,
         Combination,
         Frequency_svi1,
         Duration_svi1,
         Proportion_svi1,
         Frequency_svi2,
         Duration_svi2,
         Proportion_svi2,
         Frequency_svi3,
         Duration_svi3,
         Proportion_svi3,
         Frequency_svi4,
         Duration_svi4,
         Proportion_svi4)

write.csv(combined_data_with_total, "output-svi.csv")



