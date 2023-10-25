#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("Severe Weather"),
  sidebarLayout(
    sidebarPanel(
      selectInput("urbanicity", "Select urbanicity:",
                  choices = c("NYC","Non-NYC urban", "Rural" )),
      selectInput("severe_weather", "Select weather metric:",
                  choices = c("Precipitation","Temperature","Snowfall","Windspeed")),
      selectInput("dimension", "Select plot dimension:",
                  choices = c("2D", "3D")),
      conditionalPanel(condition = "input.dimension == '2D'",
                       checkboxGroupInput("lags", "Select lags (hourly):",
                                          choices = c("0", "1", "2", "3", "4", "5", "6", "7",
                                                      "8", "9", "10", "11", "12", "13",
                                                      "14", "15", "16", "17", "18", "19",
                                                      "20", "21", "22", "23", "24",
                                                      "All"), selected = "1"))
    ),
    mainPanel(
      conditionalPanel(condition = "input.dimension == '2D'",
                       plotOutput("plot_2d"),
                       verbatimTextOutput("plot_2d_description")),
      conditionalPanel(condition = "input.dimension == '3D'",
                       plotlyOutput("plot_3d"),
                       verbatimTextOutput("plot_3d_description"))
    )
  )
)
