#uses the data from hardiness/dashboard/mainDashboard.R

library(shiny)
library(tidyverse)
#would have to update this manually every year

setwd("C:/Ecology Lab/R/bcvin_git/bcvin/hardiness/dashboard/historicLTEdata")
`2013 to 2014` <- read_csv("predLTE_combined_2013to14.csv") 
`2014 to 2015` <- read_csv("predLTE_combined_2014to15.csv")
`2015 to 2016` <- read_csv("predLTE_combined_2015to16.csv")
`2016 to 2017` <- read_csv("predLTE_combined_2016to17.csv")
`2017 to 2018` <- read_csv("predLTE_combined_2017to18.csv")
`2018 to 2019` <- read_csv("predLTE_combined_2018to19.csv")
`2019 to 2020` <- read_csv("predLTE_combined_2019to20.csv")

#made using copyActualLTE from helpfulFunctions.R 
measuredLTE_2013to14 <- tibble(date = as.Date(c("2013-10-25", "2013-11-08", "2013-11-22", "2013-12-06", "2013-12-20", "2014-01-03", "2014-01-17", "2014-01-31", "2014-02-14", "2014-02-28", "2014-03-14", "2014-03-27"), format = "%Y-%m-%d"),
                               LTE = c(-13.9, -19.42, -22.87, -24.06, -24.09, -23.06, -23, -23.46, -22.6, -22.76, -17.62, -14.89))
measuredLTE_2014to15 <- tibble(date = as.Date(c("2014-10-28", "2014-11-11", "2014-11-24", "2014-12-08", "2014-12-22", "2015-01-05", "2015-01-19", "2015-02-02", "2015-02-16", "2015-03-04", "2015-03-18"), format = "%Y-%m-%d"), 
                               LTE = c(-13.58, -18.26, -21.3, -22.5, -22.67, -23.6, -22.12, -22.17, -18.5, -18.82, -11.55))
measuredLTE_2015to16 <- tibble(date = as.Date(c("2015-10-27", "2015-11-10", "2015-11-24", "2015-12-08", "2015-12-22", "2016-01-05", "2016-01-19", "2016-02-02", "2016-02-16", "2016-03-01", "2016-03-15", "2016-03-29"), format = "%Y-%m-%d"), 
                               LTE = c(-15.76, -20.52, -22.51, -22.77, -22.99, -24.21, -23.38, -22.84, -21.92, -18, -14.99, -11.16))
measuredLTE_2016to17 <- tibble(date = as.Date(c("2016-11-08", "2016-11-22", "2016-12-06", "2016-12-20", "2017-01-03", "2017-01-17", "2017-01-31", "2017-02-14", "2017-02-28", "2017-03-14", "2017-03-28", "2017-04-04"), format = "%Y-%m-%d"), 
                               LTE = c(-17.25, -20.98, -23.49, -24.42, -24.69, -26.05, -23.58, -24.08, -22.87, -20.5, -15.33, -13.66))
measuredLTE_2017to18 <- tibble(date = as.Date(c("2017-11-07", "2017-11-21", "2017-12-05", "2017-12-19", "2018-01-02", "2018-01-16", "2018-01-30", "2018-02-13", "2018-02-27", "2018-03-13", "2018-03-28", "2018-04-10"), format = "%Y-%m-%d"), 
                               LTE = c(-21.95, -22.2, -23.8, -23.6, -25.4, -23.6, -22.9, -23.6, -23.5, -20.8, -15.9, -11))
measuredLTE_2018to19 <- tibble(date = as.Date(c("2018-11-06", "2018-11-20", "2018-12-04", "2018-12-18", "2019-01-01", "2019-01-15", "2019-01-29", "2019-02-12", "2019-02-26", "2019-03-12", "2019-03-26", "2019-04-09"), format = "%Y-%m-%d"), 
                               LTE = c(-17.52, -23.3, -23.78, -23.85, -24.69, -24.02, -24.79, -25.18, -25.33, -24.65, -16.97, -10.72))
measuredLTE_2019to20 <- tibble(date = as.Date(c("2019-10-24", "2019-11-07", "2019-11-21", "2019-12-05", "2019-12-19", "2020-01-07", "2020-01-17", "2020-01-30", "2020-02-13", "2020-02-27", "2020-03-12", "2020-03-26"), format = "%Y-%m-%d"),
                               LTE = c(-17.4, -21.19, -22.2, -24.4, -24.12, -23.34, -24.31, -22.8, -22.45, -21.31, -18.16, -15.83))

`2013 to 2014_measured` <- measuredLTE_2013to14
`2014 to 2015_measured` <- measuredLTE_2014to15
`2015 to 2016_measured` <- measuredLTE_2015to16
`2016 to 2017_measured` <- measuredLTE_2016to17
`2017 to 2018_measured` <- measuredLTE_2017to18
`2018 to 2019_measured` <- measuredLTE_2018to19
`2019 to 2020_measured` <- measuredLTE_2019to20

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput(
    inputId = "year", 
    label = "Select a dormant season",
    choices = c(
      "2013 to 2014",
      "2014 to 2015",
      "2015 to 2016",
      "2016 to 2017",
      "2017 to 2018",
      "2018 to 2019",
      "2019 to 2020"
    )
  ),
  
  plotOutput("scatterplot"),
  plotOutput("temperatures")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dat <- reactive({
    input$year
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data = get(input$year)) + 
      geom_point(mapping = aes(x = date, y = predLTE3, color = "green")) +
      geom_point(mapping = aes(x = date, y = LTE, color = "red"), data = get(paste0(input$year, "_measured"))) +
      scale_color_hue(labels = c("Predicted LTE 50", "Actual LTE 50"))+
      labs(colour = "Colours") +
      xlab("Date") +
      ylab("LTE 50") +
      ggtitle(paste0("Predicted Chardonnay Hardiness in the Dormancy period for ", input$year), 
              subtitle = "Originated from Carl's May 2020 model")
  })
  
  output$temperatures <- renderPlot({
    ggplot(data = get(input$year)) +
      geom_line(mapping = aes(x = date, y = twoDayAvg)) +
      xlab("Date") +
      ylab("Temperature (Celsius)") +
      ggtitle(paste0("Two Day Average Temperatures in the Dormancy Period for ", input$year),
              subtitle  = "Note: breaks in line are from missing data from the Penticton Weather Station") 
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
