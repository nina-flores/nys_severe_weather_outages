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
  filter(strata == "overall"| strata == "u" | strata == "r" | strata == "n") %>%
  select(combination, strata, freq, mean_duration, sd_duration,mean_prop_out, sd_prop_out) %>%
  mutate(type = if_else(str_detect(combination, "\\+"), "multiple", "single")) %>% 
  arrange(desc(type), desc(freq))%>%
  mutate(duration = paste0(round(mean_duration,1)," (",round(sd_duration,1),")"),
         prop_out = paste0(round(mean_prop_out,1)," (",round(sd_prop_out,1),")")) %>%
  mutate(freq = round(freq,1)) %>%
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
  filter(Frequency_overall >0) %>%
  mutate(Combination = if_else(Combination == "", "no severe weather", Combination))


combined_data_with_total <- bind_rows(data_pretty_urbanicity, summarise_if(data_pretty_urbanicity, is.numeric, sum))%>%
  mutate(Combination = if_else(is.na(Combination)== TRUE, "Total", Combination)) %>%
  mutate(across(where(is.numeric), ~ sprintf("%s (%.1f)", ., (. / last(.)) * 100))) %>%
  select(Type,
         Combination,
         Frequency_overall,
         Duration_overall,
         Proportion_overall,
         Frequency_n,
         Duration_n,
         Proportion_n,
         Frequency_u,
         Duration_u,
         Proportion_u,
         Frequency_r,
         Duration_r,
         Proportion_r)





# Rest of the code for creating and saving the Word document

# Create a Word document
doc <- read_docx()


# Extract the values before parentheses in the first row
first_row_values <- gsub("\\(.*\\)", "", combined_data_with_total[1,3:14])

# Create a function to compare values before parentheses
compare_values <- function(value1, value2) {

  
  # Extract the values before parentheses
  val1 <- gsub("\\(.*\\)", "", value1)
  val2 <- gsub("\\(.*\\)", "", value2)
  
  # Convert the extracted values to numeric
  val1 <- as.numeric(val1)
  val2 <- as.numeric(val2)
  
  # Compare the values and return the result
  if (val2 > val1) {
    return("#eab676")
  } else {
    return("#abdbe3")
  }
}

tbl <- flextable::flextable(combined_data_with_total)

# Apply color formatting to the table cells
for (i in 1:(nrow(combined_data_with_total) - 1)) {
  for (j in 3:14) {  # Adjust column range accordingly
    value <- combined_data_with_total[[i, j]]
    if (!is.na(value)) {
      bg_color <- compare_values(first_row_values[j - 2], value)
      tbl <- flextable::bg(tbl, i = i, j = j, bg = bg_color)
    }
  }
}

save_as_docx(tbl, path = "~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/output/table-duration-freq-prop/output.docx")

