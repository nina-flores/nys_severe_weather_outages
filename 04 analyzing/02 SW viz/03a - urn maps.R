require(sf)
require(dplyr)
require(fst)
require(ggpubr)
require(viridis)
require(scales)
require(gridExtra)
require(tidyr)
require(ggsn)
require(ggspatial)

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

### map of the nys regions to be used throughout:

shape_n <- shape %>% filter(urn == "n")
shape_r <- shape %>% filter(urn == "r")
shape_u <- shape %>% filter(urn == "u")


o <- ggplot() +
  geom_sf(data = shape, aes(fill = urn), size = 0.05) +
  theme(strip.text.x = element_text(size = 8),
        strip.background = element_rect(colour = NA, fill = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = NA, fill = NA)) +
  scale_fill_manual(values = c("#ffeda0", "#fdbb84", "#9ebcda"),
                    breaks = c("n", "u", "r"),
                    labels = c("NYC", "Urban,\nnon-NYC", "Rural"),
                    name = "") + theme(legend.position = "bottom")+
  annotation_scale()
  


n <-ggplot() +
  geom_sf(data = shape_n, aes(fill = urn), size = 0.05) +
  theme(strip.text.x = element_text(size = 8),
        strip.background = element_rect(colour = NA, fill = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = NA, fill = NA)) +
  scale_fill_manual(values = c("#ffeda0"),
                    name = "") + theme(legend.position = "none")


p <- ggplot() +
  geom_sf(data = shape_u, aes(fill = urn), size = 0.05) +
  coord_sf(crs = 4269)+
  scale_fill_manual(values = c("#fdbb84"), name = "") +
  theme_void() +
  theme(legend.position = "none")+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", size=.3)


# Inset plot
p_inset <- ggplot() +
  geom_sf(data = shape_u, aes(fill = urn), size = 0.05)  +
  scale_fill_manual(values = c("#fdbb84"), name = "") +
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


u <- p + 
  annotation_custom(
    ggplotGrob(p_inset), 
    xmin = -80, xmax = -74.5 , ymin = 38.5, ymax = 41.8)




p <- ggplot() +
  geom_sf(data = shape_r, aes(fill = urn), size = 0.05) +
  coord_sf(crs = 4269)+
  scale_fill_manual(values = c("#9ebcda"), name = "") +
  theme_void() +
  theme(legend.position = "none")+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", size=.3)


# Inset plot
p_inset <- ggplot() +
  geom_sf(data = shape_r, aes(fill = urn), size = 0.05)  +
  scale_fill_manual(values = c("#9ebcda"), name = "") +
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


r <- p + 
  annotation_custom(
    ggplotGrob(p_inset), 
    xmin = -80, xmax = -74.5, ymin = 38.5, ymax = 41.8)

a <- grid.arrange(arrangeGrob(o),
             arrangeGrob(n, u, r, ncol = 3),
             heights = c(1.5, 2),
             nrow = 2)


#where to save
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/viz-maps")
ggsave("regions.pdf", a, width = 11.5, height = 8)
