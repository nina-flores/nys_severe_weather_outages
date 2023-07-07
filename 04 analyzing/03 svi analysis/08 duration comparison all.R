
require(tidyverse)
require(data.table)
require(dplyr)
require(sf)
require(fst)
require(lubridate) 
require(runner)
require(ggplot2)
#install.packages("ggpubr", dependencies = TRUE)
require(ggpubr)
#install.packages("scales", dependencies = TRUE)
require(scales)
library(ggpubr)



setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators")
outages_n_dur_long <- read.fst("outages_n_dur_long.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>% 
  mutate(svi_bin = ifelse(svi_quartile == "Q4", "Most vulnerable \n 4th quartile SVI", "SVI quartiles 1-3")) %>%
  drop_na(svi_bin)
outages_u_dur_long <- read.fst("outages_u_dur_long.fst")%>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>% 
  mutate(svi_bin = ifelse(svi_quartile == "Q4", "Most vulnerable \n 4th quartile SVI", "SVI quartiles 1-3"))%>%
  drop_na(svi_bin)
outages_r_dur_long <- read.fst("outages_r_dur_long.fst") %>%
  mutate(svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>% 
  mutate(svi_bin = ifelse(svi_quartile == "Q4", "Most vulnerable \n 4th quartile SVI", "SVI quartiles 1-3")) %>%
  drop_na(svi_bin)

### 1b. Create a function to plot each

sw_variable = c("cold_driven_outage" ,"heat_driven_outage", "lightning_driven_outage",
                "precipitation_driven_outage",  "snow_driven_outage","wind_driven_outage")

plots_list <- lapply(sw_variable, function(swv){
  
  ylabel = case_when(swv == "cold_driven_outage" ~ "Cold driven outage",
                     swv == "heat_driven_outage" ~ "Heat driven outage",
                     swv == "lightning_driven_outage" ~ "Lightning driven outage",
                     swv == "precipitation_driven_outage" ~"Precipitation driven outage",
                     swv == "snow_driven_outage" ~ "Snow driven outage",
                     swv == "wind_driven_outage" ~ "Wind driven outage") 
  
  data =  outages_n_dur_long %>% filter(name == swv)
  
  p = ggviolin(data, x = "svi_quartile", y = "duration", fill = "svi_quartile",
               alpha = .5,
               add = "boxplot", add.params = list(fill = "white"), 
               legend.title = "", legend = "none", xlab = FALSE, ylab = "Duration",ggtheme = theme_minimal()) +
    scale_y_log10() + 
    ggtitle(ylabel) + 
    scale_fill_brewer(palette = "Blues")
  
  p_with_comparisons <- p + 
    ggpubr::geom_pwc(ggplot2::aes(group = svi_quartile), data = data, tip.length = 0,
                     method = "t_test", label = "p.format")
  
  return(p_with_comparisons)
  
})

plot1 <- plots_list[[1]]
plot2 <- plots_list[[2]]
plot3 <- plots_list[[3]]
plot4 <- plots_list[[4]]
plot5 <- plots_list[[5]]
plot6 <- plots_list[[6]]

  
  



### 1c. Plot all together

plot_nyc <- ggarrange(plot1, plot2, plot3, plot4,plot5,plot6,
                      labels = c("a", "b", "c", "d", "e", "f"))
n <- annotate_figure(plot_nyc, top = text_grob("NYC"))




#############################################################################
### 1b. Create a function to plot each

sw_variable = c("cold_driven_outage" ,"heat_driven_outage", "lightning_driven_outage",
                "precipitation_driven_outage",  "snow_driven_outage","wind_driven_outage")

plots_list <- lapply(sw_variable, function(swv){
  
  ylabel = case_when(swv == "cold_driven_outage" ~ "Cold driven outage",
                     swv == "heat_driven_outage" ~ "Heat driven outage",
                     swv == "lightning_driven_outage" ~ "Lightning driven outage",
                     swv == "precipitation_driven_outage" ~"Precipitation driven outage",
                     swv == "snow_driven_outage" ~ "Snow driven outage",
                     swv == "wind_driven_outage" ~ "Wind driven outage") 
  
  data =  outages_u_dur_long %>% filter(name == swv)
  
  p = ggviolin(data, x = "svi_quartile", y = "duration", fill = "svi_quartile",
               alpha = .5,
               add = "boxplot", add.params = list(fill = "white"), 
               legend.title = "", legend = "none", xlab = FALSE, ylab = "Duration",ggtheme = theme_minimal())+
    scale_y_log10() +   
    stat_compare_means(label.y.npc = 1, label.x.npc = .3)+
    ggtitle(ylabel)+ scale_fill_brewer(palette = "Blues")
  
  p_with_comparisons <- p + 
    ggpubr::geom_pwc(ggplot2::aes(group = svi_quartile), data = data, tip.length = 0,
                     method = "t_test", label = "p.format")
  
  return(p_with_comparisons)
  
  
})


plot1 <- plots_list[[1]]
plot2 <- plots_list[[2]]
plot3 <- plots_list[[3]]
plot4 <- plots_list[[4]]
plot5 <- plots_list[[5]]
plot6 <- plots_list[[6]]


### 1c. Plot all together

plot_u <- ggarrange(plot1, plot2, plot3, plot4,plot5,plot6,
                    labels = c("a", "b", "c", "d", "e", "f"))
u <- annotate_figure(plot_u, top = text_grob("Urban, non-NYC"))



#############################################################################
### 1b. Create a function to plot each

sw_variable = c("cold_driven_outage" ,"heat_driven_outage", "lightning_driven_outage",
                "precipitation_driven_outage",  "snow_driven_outage","wind_driven_outage")

plots_list <- lapply(sw_variable, function(swv){
  
  ylabel = case_when(swv == "cold_driven_outage" ~ "Cold driven outage",
                     swv == "heat_driven_outage" ~ "Heat driven outage",
                     swv == "lightning_driven_outage" ~ "Lightning driven outage",
                     swv == "precipitation_driven_outage" ~"Precipitation driven outage",
                     swv == "snow_driven_outage" ~ "Snow driven outage",
                     swv == "wind_driven_outage" ~ "Wind driven outage") 
  
  data =  outages_r_dur_long %>% filter(name == swv)
  
  p = ggviolin(data, x = "svi_quartile", y = "duration", fill = "svi_quartile",
               alpha = .5,
               add = "boxplot", add.params = list(fill = "white"), 
               legend.title = "", legend = "none", xlab = FALSE, ylab = "Duration",ggtheme = theme_minimal())+
    scale_y_log10() +   
    stat_compare_means(label.y.npc = 1, label.x.npc = .3)+
    ggtitle(ylabel)+ scale_fill_brewer(palette = "Blues")
  
  
  p_with_comparisons <- p + 
    ggpubr::geom_pwc(ggplot2::aes(group = svi_quartile), data = data, tip.length = 0,
                     method = "t_test", label = "p.format")
  
  return(p_with_comparisons)
  
  
})


plot1 <- plots_list[[1]]
plot2 <- plots_list[[2]]
plot3 <- plots_list[[3]]
plot4 <- plots_list[[4]]
plot5 <- plots_list[[5]]
plot6 <- plots_list[[6]]


### 1c. Plot all together

plot_r <- ggarrange(plot1, plot2, plot3, plot4,plot5,plot6,
                    labels = c("a", "b", "c", "d", "e", "f"))
r<- annotate_figure(plot_r, top = text_grob("Rural"))




