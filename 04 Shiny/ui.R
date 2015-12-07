#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ST Ratio's Scatterplots", tabName = "Scatterplot", icon = icon("dashboard")),
      menuItem("Barchart", tabName = "barchart", icon = icon("th")),
      menuItem("Blending", tabName = "blending", icon = icon("th")),
      menuItem("Map", tabName = "map", icon = icon("th")),
      menuItem("Table", tabName = "table", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Scatterplot",
        
              actionButton(inputId = "clicks1",  label = "Show Scatter Plot 1"),
              actionButton(inputId = "clicks2",  label = "Show Scatter Plot 2"),
              plotOutput("distPlot1"),
              plotOutput("distPlot2")
      )
 
      ,
      
      # Third tab content
      tabItem(tabName = "blending",
              actionButton(inputId = "clicks",  label = "Click me"),
              plotOutput("distPlot3")
      ),
      
      # Fourth tab content
      tabItem(tabName = "map",
              leafletOutput("map")
      ),
      
      # Fifth tab content
      tabItem(tabName = "table",
              dataTableOutput("table")
      )
    )
  )
)
