require(fst)
require(data.table)
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/NLDAS_utility")
dta <- fread("NLDAS_2017_2020_1400.csv")
write_fst(dta, "NLDAS_utility_1400.fst")

