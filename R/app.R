library(shiny)
library(DBI)
library(ggplot2)
library(dbplyr)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  titlePanel(title=h4("Export trend by commodity", align="center")),
  sidebarLayout(
    sidebarPanel("Menu",
                 selectInput("select", "Select Commodity", choices = c("A-Copra" = "A-Copra", "B-Coconut oil"="B-Coconut oil", "C-beef"="C-beef", "D-Cocoa"="D-Cocoa", "	
E-Shell"="E-Shell", "F-sawn timber"="F-sawn timber"))
                 
    ),
    
    
    mainPanel(
      
      plotlyOutput("myplotly")
      
    )
    
  )
)

server <- function(input, output, session) {
  
  output$myplotly <- renderPlotly({
    #Establish conneciton to the Mysql database
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "trade",
      host = "localhost",
      username = "root",
      password = "")
    #Close connection once the application is closed
    on.exit(dbDisconnect(conn), add = TRUE)
    mydat <- dbGetQuery(conn, paste0(
      "SELECT Year, Value_mill FROM tab WHERE AlphOrder = '", input$select,"'"))
    ggplot(mydat, aes(x=Year, y=Value_mill, colour = Value_mill)) +
      geom_line() +
      geom_point() +
      geom_smooth()
  })
  
}

shinyApp(ui, server)