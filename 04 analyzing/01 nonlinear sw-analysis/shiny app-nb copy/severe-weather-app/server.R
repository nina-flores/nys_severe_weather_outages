#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(fst)
library(plotly)


# Determine the absolute path to the data directory
data_dir <- here::here() 
data <- read.fst(file.path(data_dir, "data/data.fst"))
f <- read.fst(file.path(data_dir, "data/overall_dta_medians.fst"))

temperature_median <- round(f$temperature_md,2)
total_precipitation_median <- round(f$total_precipitation_md,2)
snowfall_hourly_median <- round(f$snowfall_hourly_md,2)
abs_wind_speed_knots_median <- round(f$abs_wind_speed_knots_md,2)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Helper function to subset data based on lags selected
  
  
  
  lag_subset <- reactive({
      if ("All" %in% input$lags) {
        selected_lags <- 0:24 # If "All" is selected, set lags to 1 to 24
      } else {
        selected_lags <- input$lags # Otherwise, set lags to user input
      }
      return(selected_lags)
    })
  
  
  a_reactive <- reactive({
    if (input$severe_weather == "Precipitation") {
      return(total_precipitation_median)
    } else if (input$severe_weather == "Temperature") {
      return(temperature_median)
    } else if (input$severe_weather == "Windspeed") {
      return(abs_wind_speed_knots_median)
    } else if (input$severe_weather == "Snowfall") {
      return(snowfall_hourly_median)
    }
  })
  
  
  output$plot_2d_description <- renderPrint({
    description <- HTML(paste("The relationship between hourly weather metrics and   
    the proportion of customers without power in a  power operating locality.   
    Here, this plot shows this relationship for",input$severe_weather,"in",input$urbanicity, 
                              
    
    "localities of NYS, during the selected lag hour(s). All rate ratios are   
    calculated with respect to the median of the input weather metric.   
    That is, rate ratios can be interpreted as the rate of customers without   
    power at a certain temperature and lag compared to that lag   
    at the median temperature. The median temperature is shown by the dashed, blue
    vertical line. "))
    description
  })
  
  output$plot_3d_description <- renderPrint({
    description <- HTML(paste("The relationship between hourly weather metrics, the 
    proportion of customers without power, and 24 hours of lags in a  power 
    operating locality. Here, this plot shows this relationship for,"
    ,input$severe_weather,"in",input$urbanicity,"localities of NYS, with up
    to 24 hours of lagged weather metrics. All rate ratios are calculated
    with respect to the median of the input weather metric. That is, 
    rate ratios can be interpreted as the rate of customers without power at a 
    certain temperature and lag compared to that lag at the median temperature. 
    The overall median", input$severe_weather ,"was", a_reactive()))
    description
  })




  # Generate plot for the "Plot" tab
  output$plot_2d <- renderPlot({
    # Generate 2D plot based on user input
    
    data_u <- data %>% filter(urbanicity  == "u")
    data_r <- data %>% filter(urbanicity  == "r")
    data_n <- data %>% filter(urbanicity  == "n")
    
    data <- switch(input$urbanicity,
                   "Non-NYC urban" = data_u,
                   "Rural" = data_r,
                   "NYC" = data_n)
    
  
 
    
    if (input$severe_weather == "Precipitation") {
      data = data %>% filter(severe_weather_variable == "total_precipitation" )%>% filter(lag %in% lag_subset() )
        ggplot(data) + 
          geom_line(aes(variable, fit.rr), color= "black") + 
          geom_ribbon(aes(x= variable, ymin= (lci.rr),  ymax = (uci.rr)), fill = "black",  alpha  = 0.20)+
          geom_hline(yintercept = 1, color= "black", linetype = "dashed") +
          geom_vline(xintercept = f$total_precipitation_md, color = "blue", linetype = "dashed") +
          facet_wrap(.~lag)+ theme_minimal()+
          ggtitle(paste("Precipitation and the proportion of customers without power (", input$urbanicity,")"))+
          ylab("Relative rate of customers without power (compared to median)") +
          xlab("Precipitation (mm)")
        
         
        
      }
     else if (input$severe_weather == "Temperature") {
      data = data %>% filter(severe_weather_variable == "temperature" )%>% filter(lag %in% lag_subset() )
        ggplot(data) + 
          geom_line(aes(variable, fit.rr), color= "black") + 
          geom_ribbon(aes(x= variable, ymin= (lci.rr),  ymax = (uci.rr)), fill = "black",  alpha  = 0.20)+
          geom_hline(yintercept = 1, color= "black", linetype = "dashed") +
          geom_vline(xintercept = f$temperature_md, color = "blue", linetype = "dashed") +
          facet_wrap(.~lag)+ theme_minimal()+
          ggtitle(paste("Temperature and the proportion of customers without power (", input$urbanicity,")"))+
          ylab("Relative rate of customers without power (compared to median)") +
          xlab("Temperature (C)")
     
    } else if (input$severe_weather == "Snowfall") {
      data = data %>% filter(severe_weather_variable == "snowfall_hourly" )%>% filter(lag %in% lag_subset() )
        ggplot(data) + 
          geom_line(aes(variable, fit.rr), color= "black") + 
          geom_ribbon(aes(x= variable, ymin= (lci.rr),  ymax = (uci.rr)), fill = "black",  alpha  = 0.20)+
          geom_hline(yintercept = 1, color= "black", linetype = "dashed") +
          geom_vline(xintercept = f$snowfall_hourly_md, color = "blue", linetype = "dashed") +
          facet_wrap(.~lag)+ theme_minimal()+
          ggtitle(paste("Snowfall and the proportion of customers without power (", input$urbanicity,")"))+
          ylab("Relative rate of customers without power (compared to median)") +
          xlab("Snowfall (mm)")
      
    } else if (input$severe_weather == "Windspeed") {
      data = data %>% filter(severe_weather_variable == "abs_wind_speed_knots" )%>% filter(lag %in% lag_subset() )
        ggplot(data, aes(x = variable, y = fit.rr)) + 
          geom_line(aes(variable, fit.rr), color= "black") + 
          geom_ribbon(aes(x= variable, ymin= (lci.rr),  ymax = (uci.rr)), fill = "black",  alpha  = 0.20)+
          geom_hline(yintercept = 1, color= "black", linetype = "dashed") +
          geom_vline(xintercept = f$abs_wind_speed_knots_md, color = "blue", linetype = "dashed") +
          facet_wrap(.~lag)+ theme_minimal()+
          ggtitle(paste("Windspeed and the proportion of customers without power (", input$urbanicity,")"))+
          ylab("Relative rate of customers without power (compared to median)") +
          xlab("Windspeed (kts)")
      
  }
    })
  
  # Generate 3D plot for the "Plot" tab
  output$plot_3d <- renderPlotly({
    # Generate 3D plot based on user input
    
    data_u <- data %>% filter(urbanicity  == "u") %>%
      mutate(RR = fit.rr)
    data_r <- data %>% filter(urbanicity  == "r")%>%
      mutate(RR = fit.rr)
    data_n <- data %>% filter(urbanicity  == "n")%>%
      mutate(RR = fit.rr)
    
    
    data <- switch(input$urbanicity,
                   "Non-NYC urban" = data_u,
                   "Rural" = data_r,
                   "NYC" = data_n)
    
    if (input$severe_weather == "Precipitation") {
      data = data %>% filter(severe_weather_variable == "total_precipitation")
      plot_ly(data = data, x = ~variable, y = ~lag, z = ~RR, type = "mesh3d",
              intensity = ~RR,
              colorscale = list(c(0,1), c("#9ecae1","#756bb1"))) %>%
        layout(scene = list(xaxis = list(title = "Precipitation (mm)"),
                            yaxis = list(title = "Lag"),
                            zaxis = list(title = "Relative rate without power")))
    } else if (input$severe_weather == "Temperature") {
      data = data %>% filter(severe_weather_variable == "temperature")
      plot_ly(data = data, x = ~variable, y = ~lag, z = ~RR, type = "mesh3d",
              intensity = ~RR,
              colorscale = list(c(0,1), c("#9ecae1","#756bb1"))) %>%
        layout(scene = list(xaxis = list(title = "Temperature (C)"),
                            yaxis = list(title = "Lag"),
                            zaxis = list(title = "Relative rate without power")))
    } else if (input$severe_weather == "Windspeed") {
      data = data %>% filter(severe_weather_variable == "abs_wind_speed_knots")
      plot_ly(data = data, x = ~variable, y = ~lag, z = ~RR, type = "mesh3d",
              intensity = ~RR,
              colorscale = list(c(0,1), c("#9ecae1","#756bb1"))) %>%
        layout(scene = list(xaxis = list(title = "Windspeed (kts)"),
                            yaxis = list(title = "Lag"),
                            zaxis = list(title = "Relative rate without power")))
    } else if (input$severe_weather == "Snowfall") {
      data = data %>% filter(severe_weather_variable == "snowfall_hourly")
      plot_ly(data = data, x = ~variable, y = ~lag, z = ~RR, type = "mesh3d",
              intensity = ~RR,
              colorscale = list(c(0,1), c("#9ecae1","#756bb1"))) %>%
        layout(scene = list(xaxis = list(title = "Snowfall (mm)"),
                            yaxis = list(title = "Lag"),
                            zaxis = list(title = "Relative rate without power")))
  }
} )
  
}

  
  




