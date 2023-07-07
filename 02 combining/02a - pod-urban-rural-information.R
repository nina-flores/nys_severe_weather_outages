####***********************
#### Code Description ####
# Author: Alex
# Date: 9/12/2022
# Last Update: 10/25/22
# Goal: Assign PODs Urban/Rural Using 2010 Block Data
####**********************
 
# Load packages (read_libraries)
# Load specific package for this process 
# Documentation: https://cran.r-project.org/web/packages/areal/vignettes/areal.html

require(tidyverse)
require(dplyr)
require(areal)
require(sf)
require(fst)
 
####**************************
#* Step 1: 
#* Goal: Load data required
####**************************
 
#### DPS NYSPO Data Shape File ####
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/e_locality")
locality <- st_read("e_locality.shp") %>% 
  janitor::clean_names() %>% 
  rename(dps_id = prime_dps) %>%
  select(dps_id, geometry) # 1865 rows 
 
### Block Data ###
 
## 
#   Citation: 
#            Steven Manson, Jonathan Schroeder, David Van Riper, Tracy Kugler, 
#            and Steven Ruggles. IPUMS National Historical Geographic Information System: 
#            Version 16.0 [dataset]. Minneapolis, MN: IPUMS. 2021. 
#            http://doi.org/10.18128/D050.V16.0
##
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/data-for-urban-rural-classification/nhgis0002_shapefile_tl2010_360_block_2010")
ny_blocks <- st_read("NY_block_2010.shp") %>% 
  janitor::clean_names() # 347,985 rows 
 
### Population ### (Source same as above)
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/data-for-urban-rural-classification")
census_blocks <- read_csv("nhgis0002_ds172_2010_block.csv") %>% 
  janitor::clean_names() %>% # 350,169 rows 
  select(gisjoin, urbrurala, h7v001) # Pulls unique identifier to combine with the shape file, urban/rural status, and total population 
 
####**************************
#* Step 2: 
#* Goal: Create separate urban/rural populations and assign to the census block shape file 
####**************************
 
census_blocks <- census_blocks %>% 
  mutate(urban_pop = ifelse(urbrurala == "U", h7v001, 0)) %>% 
  mutate(rural_pop = ifelse(urbrurala == "R", h7v001, 0)) 
 
# Test that total population is preserved 
# (sum(census_blocks$urban_pop) + sum(census_blocks$rural_pop)) == sum(census_blocks$h7v001) # TRUE 
 
blocks <- left_join(ny_blocks, census_blocks, by="gisjoin") %>% 
  rename(blocks = gisjoin) %>%  # 347,985 blocks 
  select(geoid10, blocks,urbrurala,h7v001,urban_pop,rural_pop, geometry)
 
####**************************
#* Step 3: 
#* Goal: Interpolate block-level population to PODs 
####**************************
 
### Set up CRS for intersection ### 
 
st_crs(blocks)
st_crs(locality)
locality <- st_transform(locality, crs = 3748)
blocks <- st_transform(blocks, crs = 3748)
st_crs(blocks) == st_crs(locality) # TRUE 
 
# Perform the intersection and step-by-step interpolation
 
# Intersection 
 
nys <- locality %>% 
  aw_intersect(blocks, areaVar = "area") 
 
# Calculate total area 
nys_pt1 <- nys %>%
  aw_total(source = blocks, id = blocks, areaVar = "area", totalVar = "totalArea",
           type = "extensive", weight = "sum") 
 
# Calculate weight 
nys_pt2 <- nys_pt1 %>%
  aw_weight(areaVar = "area", totalVar = "totalArea", 
            areaWeight = "areaWeight") 
 
# Convert to dataframe, remove shape
nys_pt3 <- as.data.frame(nys_pt2 %>% 
                           select(-geometry)) 
 
# Multiply the values of interest (counts) by weights and sum
# to get aggregate POD values 
 
pods_ur <- nys_pt3 %>% 
  mutate(pod_population = areaWeight * h7v001) %>% 
  mutate(urban_population = areaWeight * urban_pop) %>% 
  mutate(rural_population = areaWeight * rural_pop) %>% 
  group_by(dps_id) %>% 
  mutate(pod_population = sum(pod_population)) %>% 
  mutate(urban_population = sum(urban_population)) %>% 
  mutate(rural_population = sum(rural_population)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(dps_id, pod_population, urban_population, rural_population)

# How much of NY is urban/rural 
# sum(census_blocks$rural_pop)/sum(pods_ur$pod_population) 12.1%
# sum(census_blocks$urban_pop)/sum(pods_ur$pod_population) 87.9%
 
# Assign urban rural based on the following paper's definition:
 
#   Deziel, N.C., Warren, J.L., Bravo, M.A., Macalintal, F., Kimbro, 
#   R.T., Bell, M.L., 2022. Assessing community-level exposure to 
#   social vulnerability and isolation: spatial patterning and urban-rural
#   differences. Journal of Exposure Science & Environmental Epidemiology.. 
#   doi:10.1038/s41370-022-00435-8
 
pods_ur <- pods_ur %>% 
  mutate(urban_rural = ifelse((urban_population/pod_population) >= .5, "u","r")) 


# If NYC-specific designation is wanted:
 

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/locality-info")
 
nyc_pods <- read_csv("xwalk_dps_clean.csv") %>% 
  filter(county == "New York" | county == "Bronx" | county == "Kings" | county == "Queens" | 
           county == "Richmond") %>% 
  mutate(dps_id = str_remove(dps_id, '\\..*')) %>%
  mutate(urn = "n") %>% 
  select(dps_id, urn) %>% 
  filter(!duplicated(dps_id))


# combine:
pods_ur_filtered <- pods_ur %>% 
  filter(!dps_id %in% nyc_pods$dps_id) %>% 
  mutate(urn = urban_rural)%>%
  select(dps_id, urn)

pods_urn <- rbind(pods_ur_filtered, nyc_pods)



### write out
write.csv(pods_urn, "urban_rural_nyc_pods.csv")


