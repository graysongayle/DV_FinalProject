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
      menuItem("# of Ch. out of School BC", tabName = "Barchart", icon = icon("th")),
      menuItem("CrossTab", tabName = "CrossTab", icon = icon("th"))

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
      tabItem(tabName = "Barchart",
              actionButton(inputId = "clicks3",  label = "Show Bar Chart 1"),
              actionButton(inputId = "clicks4",  label = "Show Bar Chart 2"),
              plotOutput("distPlot3"),
              plotOutput("distPlot4")
      ),
      
      # Fourth tab content
      tabItem(tabName = "CrossTab",
              sidebarPanel(
                
                sliderInput("KPI1", "Difference between literacy rates of 2010 and 2000", 
                            min = -30, max = 30,  value = 0),
            
              
                actionButton(inputId = "clicks5",  label = "Click me")
              ),
              plotOutput("distPlot5")
      ),
      
      # Fifth tab content
      tabItem(tabName = "table",
              dataTableOutput("table")
      )
    )
  )
)
