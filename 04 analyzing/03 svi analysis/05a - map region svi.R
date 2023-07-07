require(tidyverse)
require(dplyr)
require(sf)
require(fst)
require(ggplot2)
require(cowplot)
require(RColorBrewer)
require(viridis)
require(BAMMtools)
require(ggpubr)
require(gridExtra)


# read in geometry file:
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/e_locality")
shape <- st_read("e_locality.shp")%>%
  mutate(dps_id = PRIME_DPS_) %>%
  select(dps_id)


setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/locality-info")
pods<- read.csv("locality-info.csv") %>%
  select(-1)

shape <- shape %>%
  full_join(pods) %>%
  drop_na(urn) %>%
  group_by(urn) %>%
  mutate(svi_quartile = as.character(ntile(svi, 4)))

data_n <- shape %>% filter(urn == "n") %>%st_transform(4269)
data_u <- shape %>% filter(urn == "u") %>% st_transform(4269)
data_r <- shape %>% filter(urn == "r")%>% st_transform(4269)


labels_svi <- c(paste('1st quartile - lowest vulnerablity'),
                paste('2'),
                paste('3'),
                paste('4th quartile - highest vulnerability'))


shape$svi_quartile <- factor(shape$svi_quartile , levels = c("1",
                                                           "2", 
                                                           "3", 
                                                           "4"),
                            labels = labels_svi )

#plot svi quartile - NYC
n <- data_n %>% 
  ggplot(aes(fill=svi_quartile)) + 
  coord_sf(crs = 4269, expand = FALSE) +
  geom_sf(size = .05) +
  theme_void() +
  scale_fill_brewer(name = "SVI quartile", palette ="Blues",
                    na.value = "grey",labels = c( labels_svi, "No data"))+
  ggtitle("NYC")

#plot svi quartile - urban, non-NYC

u1 <- data_u %>% 
  ggplot(aes(fill=svi_quartile)) + 
  coord_sf(crs = 4269, expand = FALSE)+
  geom_sf(size = .05) +
  theme_void() +
  scale_fill_brewer(name = "SVI quartile", palette ="Blues",
                    na.value = "grey",labels = c( labels_svi, "No data"))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", linewidth=.3)


# Inset plot
u2 <- ggplot() +
  geom_sf(data = data_u, aes(fill = svi_quartile), size = 0.05)  + 
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  scale_fill_brewer(name = "SVI quartile", palette ="Blues",
                    na.value = "grey",labels = c( labels_svi, "No data")) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")


u <- u1 + 
  annotation_custom(
    ggplotGrob(u2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("Urban, non-NYC")

#plot svi quartile - rural

r1 <- data_r %>% 
  ggplot(aes(fill=svi_quartile)) + 
  coord_sf(crs = 4269, expand = FALSE)+
  geom_sf(size = .05) +
  theme(strip.text.x = element_text(size=5),
        strip.background = element_rect(colour=NA, fill=NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = NA, fill = NA)) + 
  scale_fill_brewer(name = "SVI quartile", palette ="Blues",
                    na.value = "grey",labels = c( labels_svi, "No data"))+
  geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
            fill=NA, color="black", size=.3)

# Inset plot
r2 <- ggplot() +
  geom_sf(data = data_r, aes(fill = svi_quartile), size = 0.05)  + 
  coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE)+
  scale_fill_brewer(name = "SVI quartile", palette ="Blues",
                    na.value = "grey",labels = c( labels_svi, "No data")) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), legend.position = "none")

r <- r1 + 
  annotation_custom(
    ggplotGrob(r2), 
    xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)+
  ggtitle("Rural")




a <- ggarrange(n,u,r, ncol  = 3,
          common.legend = TRUE)

#where to save
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/svi-analysis")
pdf("svi-by-region.pdf",  height= 4.5, width = 10)
a
dev.off()

