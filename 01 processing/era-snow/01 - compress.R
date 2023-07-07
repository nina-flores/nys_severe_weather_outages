require(fst)
require(data.table)
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/extreme weather data/era-snow")
dta <- fread("snow_utility-ts.csv")
write_fst(dta, "snow_utility.fst")

