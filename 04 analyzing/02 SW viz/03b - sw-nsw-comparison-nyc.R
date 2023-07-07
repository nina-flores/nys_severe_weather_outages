require(sf)
require(dplyr)
require(fst)
require(ggpubr)
require(viridis)
require(scales)
require(gridExtra)
require(data.table)
require(tidyr)


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_n <- read.fst("outages_n_duration.fst") 
outages_n_defined <- read.fst("sw_outages_n_defined.fst")

outages_n <-full_join(outages_n,outages_n_defined) %>%
  filter(i_gt_90pct == 1)


# read in geometry file:
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/e_locality")
shape <- st_read("e_locality.shp")%>%
  mutate(dps_id = PRIME_DPS_) %>%
  select(dps_id)


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/locality-info")
pods_urn <- read.csv("urban_rural_nyc_pods.csv") %>%
  select(-1)

shape <- shape %>%
  full_join(pods_urn) %>%
  drop_na(urn) %>%
  filter(urn =="n")

data_n = full_join(shape, outages_n)

data_n_customers_out = data_n %>%
  select(ind_90pct, dps_id, prop_out, start_outage, urn)

data_n_duration = data_n %>%
  select(ind_90pct, dps_id, duration, start_outage, urn)%>% 
  filter(start_outage == 1)

data_n_n = data_n %>%
  select(ind_90pct, dps_id, start_outage, urn)%>% 
  filter(start_outage == 1)


###first get sw driven information:

data_n_filt <- data_n %>% filter(start_outage == 1)
setDT(data_n_filt)
data_n_filt = data_n_filt[, sw := ifelse(rowSums(.SD, na.rm = TRUE) >= 1, "Severe weather driven", "Non-severe weather driven "), 
                          .SDcols = c("cold_driven_outage", "heat_driven_outage", "wind_driven_outage",   
                                      "precipitation_driven_outage", "snow_driven_outage", "lightning_driven_outage")]

data_n_filt <- data_n_filt  %>% select(ind_90pct, sw, urn)

### calculate the average percentage without power during outages by severe weather driven and not:

data_pct_co = full_join(data_n_customers_out, data_n_filt) %>%
  group_by(sw, dps_id, urn) %>%
  summarize(avg_pct_co = mean(prop_out)*100)%>% na.omit()

### calculate the average duration without power during outages by severe weather driven and not:
data_duration = full_join(data_n_duration, data_n_filt) %>%
  group_by(sw, dps_id, urn) %>%
  summarize(avg_dur = mean(duration)) 

### calculate the frequency by severe weather driven and not:
data_freq = full_join(data_n_n, data_n_filt) %>%
  group_by(sw, dps_id, urn) %>%
  summarize(freq = n())


################################################################################
### map these values
################################################################################
### frequency 

p1 = ggplot()+
  geom_sf(data = data_freq, aes(fill = freq),size = .05)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Number of outages") +
  theme(legend.position = "top",legend.text=element_text(size=8))+
  facet_wrap(.~sw)


### % out 

p2 = ggplot()+
  geom_sf(data = data_pct_co, aes(fill = avg_pct_co),size = .05)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average customers impacted (%)") +
  theme(legend.position = "top",legend.text=element_text(size=8))+
  facet_wrap(.~sw)


### duration 

p3 = ggplot()+
  geom_sf(data = data_duration, aes(fill = avg_dur),size = .05)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average duration of outages (hours)") +
  theme(legend.position = "top",legend.text=element_text(size=8))+
  facet_wrap(.~sw)


p <- ggarrange(p1,p2,p3, nrow = 3)



#where to save
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/viz-maps")
pdf("nyc_comparison_maps.pdf", width = 8, height = 11.5)
p
dev.off()
