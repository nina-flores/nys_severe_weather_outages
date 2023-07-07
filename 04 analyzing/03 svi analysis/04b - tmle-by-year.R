require(tidyverse)
require(data.table)
require(dplyr)
require(sf)
require(fst)
require(ltmle)
require(earth)

# set the working directory to the folder containing the fst files
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/data/outage-final/sw outage indicators/year-sensitivity")

# get a list of all the fst files in the directory
files <- list.files(pattern = "\\.fst$")

sev_weath <- list("sum_cold_outages_bin",
                  "sum_heat_outages_bin",
                  "sum_wind_outages_bin",
                  "sum_precip_outages_bin",
                  "sum_snow_outages_bin",
                  "sum_ltn_outages_bin")


### get the data in the correct format###

# loop over the files and apply the mutation
for (file in files) {
  # read in the file and assign it to a variable with the file name
  assign(paste0("data_", file), read.fst(file))
  
  # apply the mutation and assign it to a new variable with the file name
  assign(paste0("data_", file), 
         mutate(get(paste0("data_", file)), 
                svi_quartile = cut(svi, breaks = quantile(svi, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("Q1", "Q2", "Q3", "Q4"))) %>% 
           na.omit() %>% 
           mutate(svi_bin = ifelse(svi_quartile == "Q4", 1, 0)) %>%
           dplyr::select(utility, svi_bin, !!!(sev_weath)))
}


data_tmle <- ls(pattern = "^data_")

# create an empty data frame to store the output
output <- data.frame()

for (i in data_tmle) {
  for (sw in sev_weath) {
    
    f <- assign(paste0(i), get(paste0(i))) %>%
      dplyr::select(utility, svi_bin, sw)
    
    a <- ltmle(f,
               Ynodes = sw,
               Anodes = "svi_bin",
               Lnodes = "utility", 
               abar = list(1, 0),
               SL.library = c("SL.glm", "SL.mean", "SL.gam", "SL.earth", "SL.glmnet", "SL.xgboost"))
    
    b <- print(summary(a)) %>% as.vector()
    
    # store the output in a data frame
    temp_output <- data.frame(ATE = b$effect.measures$ATE$estimate,
                              lower_CI = b$effect.measures$ATE$CI[1],
                              upper_CI = b$effect.measures$ATE$CI[2],
                              pval = b$effect.measures$ATE$pvalue,
                              severe_weather = sw,
                              urbanicity = i)
    
    # add the output to the overall data frame
    output <- bind_rows(output, temp_output)
    
  }
}

# print the final data frame
output_data <- output %>%
  mutate(urbanicity = gsub("data_sw_outages_", "", urbanicity))%>%
  mutate(yr = substr(urbanicity, 7, 10))%>%
  mutate(urbanicity = substr(urbanicity, 1, 1))


### create a pretty plot to displey results
labels <- rev(c(paste('Cold'),
                paste('Heat'),
                paste('Lightning'),
                paste('Precipitation'),
                paste('Snow'),
                paste('Wind')))


output_data$severe_weather <- factor(output_data$severe_weather  , levels = rev(c("sum_cold_outages_bin",
                                                                        "sum_heat_outages_bin", 
                                                                        "sum_ltn_outages_bin",
                                                                        "sum_precip_outages_bin",
                                                                        "sum_snow_outages_bin",
                                                                        "sum_wind_outages_bin")),
                                labels = labels)


output_data <- output_data %>%
  mutate(per_ate = ATE*100,
         lower_CI = lower_CI*100,
         upper_CI = upper_CI*100)


output_u <- output_data %>% filter(urbanicity %in% c("u")) 
output_r <- output_data %>% filter(urbanicity %in% c("r"))
output_n <- output_data %>% filter(urbanicity %in% c("n"))



plot_u <- ggplot(output_u, aes(y = severe_weather, x = per_ate, shape = yr)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, aes(y = severe_weather, color = severe_weather)) +
  geom_errorbar(aes(xmin = lower_CI, xmax = upper_CI, color = severe_weather), width = 0.2, position = position_dodge(width = 0.6)) +  theme_minimal() +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "black")+
  ylab("")+
  xlab("")+
  theme(legend.position = "right",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+
  scale_shape_manual(values = c(1, 2, 3,4)) +
  scale_color_brewer(palette = "Dark2")+
  guides(shape = guide_legend(title = "Number of severe weather driven outages"))+  
  guides(color = "none") + 
  ggtitle("Urban, non-NYC") + xlim(-30,70)



plot_r <- ggplot(output_r, aes(y = severe_weather, x = per_ate, shape = yr)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, aes(y = severe_weather, color = severe_weather)) +
  geom_errorbar(aes(xmin = lower_CI, xmax = upper_CI, color = severe_weather), width = 0.2, position = position_dodge(width = 0.6)) +  theme_minimal() +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "black")+
  ylab("")+
  xlab("Percent difference in risk (4th quartile SVI versus all other)")+
  theme(legend.position = "right",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+
  scale_shape_manual(values = c(1, 2, 3,4)) +
  scale_color_brewer(palette = "Dark2")+
  guides(shape = guide_legend(title = "Number of severe weather driven outages"))+  
  guides(color = "none") + 
  ggtitle("Rural")+ xlim(-30,70)


plot_n <- ggplot(output_n, aes(y = severe_weather, x = per_ate, shape = yr)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, aes(y = severe_weather, color = severe_weather)) +
  geom_errorbar(aes(xmin = lower_CI, xmax = upper_CI, color = severe_weather), width = 0.2, position = position_dodge(width = 0.6)) +  theme_minimal() +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "black")+
  ylab("")+
  xlab("")+
  theme(legend.position = "right",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+
  scale_shape_manual(values = c(1, 2, 3,4)) +
  scale_color_brewer(palette = "Dark2")+
  guides(shape = guide_legend(title = "Number of severe weather driven outages"))+  
  guides(color = "none") + 
  ggtitle("NYC")+ xlim(-30,70)

require(ggpubr)
# Arrange plots side by side and give them a joint legend at the bottom
#where to save
setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/figures/svi-analysis")
a <- ggarrange(plot_n, plot_u, plot_r,  ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

pdf("tmle-results-by-year.pdf", width = 11.5, height = 8)
a 
dev.off()





