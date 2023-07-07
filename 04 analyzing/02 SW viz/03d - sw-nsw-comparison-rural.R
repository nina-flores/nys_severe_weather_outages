require(sf)
require(dplyr)
require(fst)
require(ggpubr)
require(viridis)
require(scales)
require(gridExtra)
require(data.table)
require(tidyr)
require(cowplot)


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_r <- read.fst("outages_r_duration.fst") 
outages_r_defined <- read.fst("sw_outages_r_defined.fst")

outages_r <-full_join(outages_r,outages_r_defined) %>%
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
  filter(urn =="r")%>%
  st_transform(4269)%>%
  st_make_valid()

data_r = full_join(shape, outages_r)

data_r_customers_out = data_r %>%
  select(ind_90pct, dps_id, prop_out, start_outage, urn)

data_r_duration = data_r %>%
  select(ind_90pct, dps_id, duration, start_outage, urn)%>% 
  filter(start_outage == 1)

data_r_n = data_r %>%
  select(ind_90pct, dps_id, start_outage, urn)%>% 
  filter(start_outage == 1)


###first get sw driven information:

data_r_filt <- data_r %>% filter(start_outage == 1)
setDT(data_r_filt)
data_r_filt = data_r_filt[, sw := ifelse(rowSums(.SD, na.rm = TRUE) >= 1, "Severe weather driven", "Non-severe weather driven"), 
                          .SDcols = c("cold_driven_outage", "heat_driven_outage", "wind_driven_outage",   
                                      "precipitation_driven_outage", "snow_driven_outage", "lightning_driven_outage")]

data_r_filt <- data_r_filt  %>% select(ind_90pct, sw, urn)

### calculate the average percentage without power during outages by severe weather driven and not:

data_pct_co = full_join(data_r_customers_out, data_r_filt) %>%
  group_by(sw, dps_id, urn) %>%
  summarize(avg_pct_co = mean(prop_out)*100)%>% na.omit()

### calculate the average duration without power during outages by severe weather driven and not:
data_duration = full_join(data_r_duration, data_r_filt) %>%
  group_by(sw, dps_id, urn) %>%
  summarize(avg_dur = mean(duration)) 

### calculate the frequency by severe weather driven and not:
data_freq = full_join(data_r_n, data_r_filt) %>%
  group_by(sw, dps_id, urn) %>%
  summarize(freq = n())


################################################################################
### map these values
################################################################################
### frequency 

# for severe weather 
sw_u1 <- data_freq %>% 
  filter(sw == "Severe weather driven") %>%
  ggplot(aes(fill=freq)) + 
  geom_sf(size = .05) +
  coord_sf(crs = 4269, expand = FALSE)+
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Number of outages",limits = c(0, 80))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "none",legend.text=element_text(size=8))


# inset plot
sw_u2 <- data_freq %>% 
  filter(sw == "Severe weather driven") %>%
  ggplot(aes(fill = freq))+
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Number of outages",limits = c(0, 80)) +
  theme(legend.position = "none",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


sw_p1 <- sw_u1 + 
  annotation_custom(
    ggplotGrob(sw_u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("Severe weather driven\n\n")



# for non severe weather 


nsw_u1 <- data_freq %>% 
  filter(sw == "Non-severe weather driven") %>%
  ggplot(aes(fill=freq)) + 
  geom_sf(size = .05) +
  coord_sf(crs = 4269, expand = FALSE)+
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Number of outages",limits = c(0, 80))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "top",legend.text=element_text(size=8))


# inset plot
nsw_u2 <- data_freq %>% 
  filter(sw == "Non-severe weather driven") %>%
  ggplot(aes(fill = freq))+
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Number of outages",limits = c(0, 80)) +
  theme(legend.position = "top",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


nsw_p1 <- nsw_u1 + 
  annotation_custom(
    ggplotGrob(nsw_u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("Non-severe weather driven")









### % out 

# for severe weather 


sw_u1 <- data_pct_co %>% 
  filter(sw == "Severe weather driven") %>%
  ggplot(aes(fill=avg_pct_co)) + 
  coord_sf(crs = 4269, expand = FALSE)+
  geom_sf(size = .05) +
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average customers impacted (%)",limits = c(0, 100))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "to[",legend.text=element_text(size=8))

# inset plot
sw_u2 <- data_pct_co %>% 
  filter(sw == "Severe weather driven") %>%
  ggplot(aes(fill=avg_pct_co)) + 
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average customers impacted (%)", limits = c(0, 100)) +
  theme(legend.position = "none",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


sw_p2 <- sw_u1 + 
  annotation_custom(
    ggplotGrob(sw_u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("\n\n")


# for non severe weather 



nsw_u1 <- data_pct_co %>% 
  filter(sw == "Non-severe weather driven") %>%
  ggplot(aes(fill=avg_pct_co)) + 
  coord_sf(crs = 4269, expand = FALSE)+
  geom_sf(size = .05) +
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average customers impacted (%)", limits = c(0, 100))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "top",legend.text=element_text(size=8))


# inset plot
nsw_u2 <- data_pct_co %>% 
  filter(sw == "Non-severe weather driven") %>%
  ggplot(aes(fill=avg_pct_co)) + 
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average customers impacted (%)",limits = c(0, 100)) +
  theme(legend.position = "top",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


nsw_p2 <- nsw_u1 + 
  annotation_custom(
    ggplotGrob(nsw_u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)






### duration 
# for  severe weather 

sw_u1 <- data_duration %>% 
  filter(sw == "Severe weather driven") %>%
  ggplot(aes(fill=as.numeric(avg_dur))) + 
  coord_sf(crs = 4269, expand = FALSE)+
  geom_sf(size = .05) +
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average duration of outages (hours)", limits = c(0, 90))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "none",legend.text=element_text(size=8))


# inset plot
sw_u2 = data_duration %>% 
  filter(sw == "Severe weather driven") %>%
  ggplot(aes(fill=avg_dur)) + 
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average duration of outages (hours)", limits = c(0, 90)) +
  theme(legend.position = "none",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


sw_p3 <- sw_u1 + 
  annotation_custom(
    ggplotGrob(sw_u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("\n\n")

# for  non severe weather 

nsw_u1 <- data_duration %>% 
  filter(sw == "Non-severe weather driven") %>%
  ggplot(aes(fill=avg_dur)) + 
  coord_sf(crs = 4269, expand = FALSE)+
  geom_sf(size = .05) +
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average duration of outages (hours)", limits = c(0, 90))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "top",legend.text=element_text(size=8))


# inset plot
nsw_u2 = data_duration %>% 
  filter(sw == "Non-severe weather driven") %>%
  ggplot(aes(fill=avg_dur)) + 
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "Average duration of outages (hours)", limits = c(0, 90)) +
  theme(legend.position = "top",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


nsw_p3 <- nsw_u1 + 
  annotation_custom(
    ggplotGrob(nsw_u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)






p <- ggarrange(nsw_p1, sw_p1,NULL,NULL,
               nsw_p2, sw_p2,NULL,NULL,
               nsw_p3, sw_p3,NULL,NULL,
               ncol = 2, nrow = 6,heights = c(1, 0.2,
                                              1, 0.2,
                                              1, 0.2))




#where to save
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/viz-maps")
pdf("rural_comparison_maps.pdf", width = 8, height = 11.5)
p
dev.off()
