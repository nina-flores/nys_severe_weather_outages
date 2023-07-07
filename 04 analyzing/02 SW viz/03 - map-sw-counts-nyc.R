require(sf)
require(dplyr)
require(fst)
require(ggpubr)
require(viridis)
require(scales)
require(gridExtra)

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
  drop_na(urn)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_u <- read.fst("outages_w_duration_u.fst")
outages_r <- read.fst("outages_w_duration_r.fst")
outages_n <- read.fst("outages_w_duration_n.fst")
outage_overall <- rbind(outages_u, outages_r, outages_n)


setDT(outage_overall)
outage_overall[, any_sw := ifelse(rowSums(.SD, na.rm = TRUE) >= 1, 1, 0), 
                        .SDcols = c("cold_driven_outage", "heat_driven_outage", "wind_driven_outage",   
                                    "precipitation_driven_outage", "snow_driven_outage", "lightning_driven_outage")]

outage_overall_long <- outage_overall %>%
  select(dps_id,any_sw, ends_with("driven_outage"), ind_90pct) %>%
  pivot_longer(!c(dps_id, ind_90pct), names_to = "sw_outage_type", values_to = "ind") %>%
  na.omit() 


################################################################################
### 3. overall frequency 1+ and 8+ outages out and sw frequency 1+ and 8+ outages
################################################################################


shape_split <- shape %>% filter(urn == "n")
outage_overall_long <- full_join(shape_split, outage_overall_long)


outage_overall_long$sw_outage_type = case_when(outage_overall_long$sw_outage_type == "any_sw" ~ "a. Severe weather driven",
                                               outage_overall_long$sw_outage_type == "cold_driven_outage" ~ "b. Cold driven outage",
                                               outage_overall_long$sw_outage_type == "heat_driven_outage" ~ "c. Heat driven outage",
                                               outage_overall_long$sw_outage_type == "lightning_driven_outage" ~ "d. Lightning driven outage",
                                               outage_overall_long$sw_outage_type == "precipitation_driven_outage" ~"e. Precipitation driven outage",
                                               outage_overall_long$sw_outage_type == "snow_driven_outage" ~ "f. Snow driven outage",
                                               outage_overall_long$sw_outage_type == "wind_driven_outage" ~ "g. Wind driven outage")


###a. 1 hour
outage_freq_1_any <- outage_overall_long %>%
  filter(ind == 1) %>%
  filter(sw_outage_type == "a. Severe weather driven") %>%
  group_by(dps_id, sw_outage_type, geometry) %>%
  mutate(freq1 = n()) %>%
  slice(1)



outage_freq_1_by_type <- outage_overall_long %>%
  filter(ind == 1) %>%
  filter(sw_outage_type != "a. Severe weather driven") %>%
  group_by(dps_id, sw_outage_type, geometry) %>%
  mutate(freq1 = n()) %>%
  slice(1)



p_any = ggplot()+
  geom_sf(data = outage_freq_1_any, aes(fill = freq1),size = .05)+
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "") +
  facet_wrap(.~sw_outage_type, ncol = 3)+
  geom_sf(data = shape_split,fill = "transparent",size = .05)+labs(fill = "")

p_type = ggplot()+
  geom_sf(data = outage_freq_1_by_type, aes(fill = freq1),size = .05)+
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "") +
  facet_wrap(.~sw_outage_type, ncol = 3)+
  geom_sf(data = shape_split,fill = "transparent",size = .05)+labs(fill = "")


p <- ggarrange(p_any, p_type, nrow = 2 )


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/viz-maps")
pdf("freq_sw_nyc.pdf", width = 8, height = 11.5)
p
dev.off()
