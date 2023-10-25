library(ggplot2)
library(tidyverse)
library(dplyr)
library(fst)

# read in and prep the data:
### 2d data: 

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/shiny app-nb/severe-weather-app/data/nb")
folder_path <- "~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/shiny app-nb/severe-weather-app/data/nb/"


combined_data <- data.frame()
# get a list of all fst files in the folder
file_list <- list.files(folder_path, pattern = "\\.fst$")

# loop through the file list and read in each file
for (file_name in file_list) {
  file_path <- paste0(folder_path, file_name)
  df <- read_fst(file_path)
  
  # extract urbanicity and severe weather variable from file name
  urbanicity <- substr(file_name, 1, 1)
  severe_weather_variable <- gsub("_24.fst$", "", file_name)
  severe_weather_variable <- gsub("n_", "", severe_weather_variable)
  severe_weather_variable <- gsub("u_", "", severe_weather_variable)
  severe_weather_variable <- gsub("r_", "", severe_weather_variable)
  
  
  
  # add columns to data frame
  df$urbanicity <- urbanicity
  df$severe_weather_variable <- severe_weather_variable
  
  # combine data with previously read data frames
  combined_data <- bind_rows(combined_data, df)
}



combined_data <- combined_data %>%
  mutate(variable = if_else(severe_weather_variable == "snowfall_hourly", variable*1000, variable))



setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/shiny app-nb/severe-weather-app/data")
write.fst(combined_data, "data.fst")


