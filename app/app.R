library(data.table)
library(shiny)
library(plotly)
library(highcharter)

source("apps/1_comparison.R", encoding = "UTF-8") 

# -- App 
ui <- fluidPage(title = "IMF data - countries reviewer",
                  navbarPage(title = "", id = 'IMF_Countries ',
                                  tabPanel(title = "IMF countries data comparison", reviewer_ui(id = "1")))
                )


server <- function(input, output, session) {
  callModule(server_1, id = "1")
}

shinyApp(ui, server) 
