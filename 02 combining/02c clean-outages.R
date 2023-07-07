# the objective of this script is to create a final outage dataset with the
# locality specific information

require(dplyr)
require(tidyverse)
require(fst)
require(sf)

setwd("~/Desktop/projects/casey cohort/outages x asthma/data/01 raw outage data")
rod <- read_fst("nys_power_outages.fst") %>%
  select(-percent_out) %>%
  mutate(prop_out = customers_out/customers) %>%
  mutate(prop_out = if_else(prop_out >1, 1, prop_out))

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/locality-info")
locality_info <- read.csv("locality-info.csv") %>% 
  select(-1)


# add locality info to outages

outage_data <- locality_info %>%
  full_join(rod) %>% drop_na(customers)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final")
write.fst(outage_data, "outages-and-locality-info.fst")

