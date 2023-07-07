#setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/01 processing/nldas-hourly")
#source("02_create_NLDAS_variables.R")


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/02 combining")
source("01 combine-hourly-data.R")

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/02 combining")
source("02a - pod-urban-rural-information.R")

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/02 combining")
source("02b - locality info compile.R")

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/02 combining")
source("02c clean-outages.R")

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/02 combining")
source("03 add-outages-hourly.R")

