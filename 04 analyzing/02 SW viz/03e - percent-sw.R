### the purpose of this script is to define, for each pol, the % of their 
### outages that were storm-driven


# read data
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_u <- read.fst("outages_w_duration_u.fst")
outages_r <- read.fst("outages_w_duration_r.fst")
outages_n <- read.fst("outages_w_duration_n.fst")
outage_overall <- rbind(outages_u, outages_r, outages_n) %>% 
  mutate_at(vars(cold_driven_outage,
                 heat_driven_outage,
                 lightning_driven_outage,
                 precipitation_driven_outage,
                 wind_driven_outage), ~replace_na(., 0))




non_severe_outage_hours <- outage_overall %>%
  mutate(nsw = if_else((cold_driven_outage +
           heat_driven_outage +
           lightning_driven_outage +
           precipitation_driven_outage +
           wind_driven_outage == 0),1,0)) %>%
  group_by(dps_id) %>%
    mutate(n = n())%>%
    ungroup() %>%
    filter(nsw == 1) %>%
    group_by(dps_id) %>%
    mutate(n_sw = n())%>%
    slice(1) %>%
    mutate(percent_sw = (1-(n_sw/n))*100) %>%
  select(dps_id, urn, percent_sw)

non_severe_outage_hours_u <- non_severe_outage_hours %>%
  filter(urn == "u")
non_severe_outage_hours_r <- non_severe_outage_hours %>%
  filter(urn == "r")
non_severe_outage_hours_n <- non_severe_outage_hours %>%
  filter(urn == "n")
    


################################################################################
### map this out
################################################################################

### prep data###
# read in geometry file:
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/e_locality")
shape <- st_read("e_locality.shp")%>%
  mutate(dps_id = PRIME_DPS_) %>%
  select(dps_id)

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/locality-info")
pods_urn <- read.csv("urban_rural_nyc_pods.csv") %>%
  select(-1)

shape_u <- shape %>%
  full_join(pods_urn) %>%
  drop_na(urn) %>%
  filter(urn =="u")%>%
  st_transform(4269)%>%
  st_make_valid()

data_u = full_join(shape_u, non_severe_outage_hours_u)

shape_r <- shape %>%
  full_join(pods_urn) %>%
  drop_na(urn) %>%
  filter(urn =="r")%>%
  st_transform(4269)%>%
  st_make_valid()

data_r = full_join(shape_r, non_severe_outage_hours_r)

shape_n <- shape %>%
  full_join(pods_urn) %>%
  drop_na(urn) %>%
  filter(urn =="n")%>%
  st_transform(4269)%>%
  st_make_valid()

data_n = full_join(shape_n, non_severe_outage_hours_n)

### map each ###

# nyc:

sw_p1 = ggplot()+
  geom_sf(data = data_n, aes(fill = percent_sw),size = .05)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "",limits = c(0, 100)) +
  theme(legend.position = "none",legend.text=element_text(size=8))+
  ggtitle("\n\nNYC")


# urban: 

# for severe weather 
sw_u1 <- data_u%>% 
  ggplot(aes(fill=percent_sw)) + 
  geom_sf(size = .05) +
  coord_sf(crs = 4269, expand = FALSE)+
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = "",limits = c(0, 100))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "top",legend.text=element_text(size=8))


# inset plot
sw_u2 <- data_u %>% 
  ggplot(aes(fill = percent_sw))+
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "",limits = c(0, 100)) +
  theme(legend.position = "none",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


sw_p2 <- sw_u1 + 
  annotation_custom(
    ggplotGrob(sw_u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("Non-NYC urban")


# rural: 

# for severe weather 
sw_r1 <- data_r%>% 
  ggplot(aes(fill=percent_sw)) + 
  geom_sf(size = .05) +
  coord_sf(crs = 4269, expand = FALSE)+
  theme_void() +
  scale_fill_viridis(option="magma", direction = -1,
                     name = " ",limits = c(0, 100))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)+
  theme(legend.position = "none",legend.text=element_text(size=8))


# inset plot
sw_r2 <- data_r %>% 
  ggplot(aes(fill = percent_sw))+
  geom_sf(size = .05)+
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void()+
  scale_fill_viridis(option="magma", direction = -1,
                     name = "",limits = c(0, 100)) +
  theme(legend.position = "none",legend.text=element_text(size=8))+
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


sw_p3 <- sw_r1 + 
  annotation_custom(
    ggplotGrob(sw_r2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("Rural")


p <- ggarrange(sw_p1,NULL,
               sw_p2,NULL,
               sw_p3,NULL,
               nrow = 1, widths = c(1, 0.2,
                                     1, 0.2,
                                     1, 0.2), common.legend = TRUE, legend = "bottom")


p <- annotate_figure(p, top = text_grob("Percent of all outages that were severe weather driven", size = 14))




#where to save
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/viz-maps")
pdf("percent_sw_driven.pdf", width = 11.5, height = 6)
p
dev.off()




    
    