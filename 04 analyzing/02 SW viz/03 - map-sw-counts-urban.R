require(sf)
require(dplyr)
require(fst)
require(ggpubr)
require(viridis)
require(scales)
require(gridExtra)
require(tidyverse)
require(data.table)

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


shape_split <- shape %>% filter(urn == "u")%>%
  st_transform(4269)%>%
  st_make_valid()
outage_overall_long <- full_join(shape_split, outage_overall_long)

range_values <- outage_overall_long%>%
  filter(ind == 1) %>%
  filter(urn == "u") %>%
  group_by(dps_id, sw_outage_type, geometry) %>%
  mutate(freq1 = n()) %>%
  slice(1)%>%
  ungroup() %>%
  summarise(min = min(freq1, na.rm = T), max = max(freq1, na.rm = T))



outage_overall_long$sw_outage_type = case_when(outage_overall_long$sw_outage_type == "any_sw" ~ "a. Any",
                                               outage_overall_long$sw_outage_type == "cold_driven_outage" ~ "b. Cold",
                                               outage_overall_long$sw_outage_type == "heat_driven_outage" ~ "c. Heat",
                                               outage_overall_long$sw_outage_type == "lightning_driven_outage" ~ "d. Lightning",
                                               outage_overall_long$sw_outage_type == "precipitation_driven_outage" ~"e. Precipitation",
                                               outage_overall_long$sw_outage_type == "snow_driven_outage" ~ "f. Snow",
                                               outage_overall_long$sw_outage_type == "wind_driven_outage" ~ "g. Wind")

# List of sw_outage_type values to plot
sw_outage_types <- c("a. Any", "b. Cold","c. Heat","d. Lightning","e. Precipitation", "f. Snow","g. Wind" )

# Create a list of plots for each sw_outage_type value
plots <- lapply(sw_outage_types, function(type) {
  
  # Filter data for current sw_outage_type value
  outage_freq_1_by_type <- outage_overall_long %>%
    filter(ind == 1) %>%
    filter(urn == "u") %>%
    filter(sw_outage_type == type) %>%
    group_by(dps_id, sw_outage_type, geometry) %>%
    mutate(freq1 = n()) %>%
    slice(1)
  
  # Main plot
  p_main <- ggplot() + 
    coord_sf(crs = 4269, expand = FALSE) +
    geom_sf(data = outage_freq_1_by_type, aes(fill = freq1), size = .05) +
    geom_rect(aes(xmin=-74.3, ymin=40.5, xmax=-71.8, ymax=41.4), 
              fill=NA, color="black", linewidth=.3) +
    theme_void() +
    scale_fill_viridis(option="magma", direction = -1, name = "",limits = c(range_values$min, range_values$max),
                       breaks = seq(range_values$min, range_values$max, length.out = 6),
                       labels = paste0(round(seq(range_values$min, range_values$max, length.out = 6),0))) +
    geom_sf(data = shape_split,fill = "transparent",size = .05) +
    labs(fill = "") + ggtitle(type)
  
  # Inset plot
  p_inset <- ggplot() +
    geom_sf(data = outage_freq_1_by_type, aes(fill = freq1),size = .05) +
    geom_sf(data = shape_split,fill = "transparent",size = .05)+
    coord_sf(crs = 4269, xlim = c(-74.3, -71.8), ylim = c(40.5, 41.4), expand = FALSE) +
    theme_void() +
    scale_fill_viridis(option="magma", direction = -1, name = "",limits = c(range_values$min, range_values$max),
                       breaks = seq(range_values$min, range_values$max, length.out = 6),
                       labels = paste0(round(seq(range_values$min, range_values$max, length.out = 6),0))) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = .7), 
          legend.position = "none")
  
  # Combine main and inset plots
  p_combined <- p_main + 
    annotation_custom(ggplotGrob(p_inset), 
                      xmin = -80, xmax = -74.5, ymin = 38.7, ymax = 41.8)
  
  # Return the combined plot
  return(p_combined)
})

# Combine all plots in the list using grid.arrange or cowplot::plot_grid
p <- ggarrange(NA,plots[[1]],NA,
               NA,NA,NA,
               plots[[2]],plots[[3]], plots[[4]],
               NA,NA,NA,
         plots[[5]],plots[[6]],plots[[7]],NA,NA,NA,
          ncol = 3, nrow = 6, common.legend = TRUE, 
          legend = "bottom", heights = c(1.2,.2,1,.2,1, .2))

setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/viz-maps")
pdf("freq_sw_urban.pdf", width = 11.5, height = 11.5)
p
dev.off()

