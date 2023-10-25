library(ggplot2)
library(tidyverse)
library(dplyr)
library(fst)
library(plotly)



setwd("~/Desktop/projects/casey cohort/power outages/severe weather PO NYS/code/extreme weather/shiny app-nb/severe-weather-app/data")
data <- read.fst("data.fst")

data_u <- data %>% filter(urbanicity  == "u")
data = data_u %>% filter(severe_weather_variable == "temperature") %>%
  mutate(RR = fit.rr)

fig <- plot_ly(data = data, x = ~variable, y = ~lag, z = ~RR, type = "mesh3d",
               intensity = ~RR,
               colorscale = list(c(0,1), c("#9ecae1","#756bb1")))


# Define the plane coordinates
plane_x <- c(min(data$variable), max(data$variable))
plane_z <- c(min(data$RR), max(data$RR))

# Create the plane as a single rectangle
fig <- fig %>% add_trace(
  type = "mesh3d",
  x = c(plane_x[1], plane_x[1], plane_x[2], plane_x[2]),
  y = c(0, 0, 0, 0),
  z = c(plane_z[1], plane_z[2], plane_z[2], plane_z[1]),
  i = c(0, 0),
  j = c(1, 2),
  k = c(2, 3),
  opacity = 0.2,  # Set the opacity
  colorscale = list(c(0,1), c("#FFA500","#FFA500")),
  showscale = FALSE
)







# Set layout
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "Temperature (C)"),
    yaxis = list(title = "Lag"),
    zaxis = list(title = "Relative rate without power")
  )
)

fig
