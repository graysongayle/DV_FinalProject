# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  

  df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 "select X2000, X2012
                                                                                 from edudata
                                                                                 where (indicator_name = \\\'Pupil-teacher ratio, primary\\\'
                                                                                 ) and (X2000 is not null and X2012 is not null)
                                                                                 order by X2000 desc"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gmg954', PASS='orcl_gmg954', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 "select X2000, X2012
                                                                                 from edudata
                                                                                 where (indicator_name = \\\'Pupil-teacher ratio, secondary\\\'
                                                                                 ) and (X2000 is not null and X2012 is not null)
                                                                                 order by X2000 desc"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gmg954', PASS='orcl_gmg954', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot1 <- renderPlot({             
    
    plot1 <- 
      ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Student-Teacher Ratio for Primary Schools from 2000 to 2012') +
      labs(x=paste("Year 2000"), y=paste("Year 2012")) +
      layer(data=df1(), 
            mapping=aes(x=X2000, y=X2012), 
            stat="identity", 
            #stat_params=list(binwidth = 0.5), 
            geom="point",
            #geom_params=list(colour=NA), 
            position = position_identity()
      )  
    
    plot1
  }) 
  
  output$distPlot2 <- renderPlot({             
    
    plot2 <- 
      ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Student-Teacher Ratio for Primary Schools from 2000 to 2012') +
      labs(x=paste("Year 2000"), y=paste("Year 2012")) +
      layer(data=df2(), 
            mapping=aes(x=X2000, y=X2012), 
            stat="identity", 
            #stat_params=list(binwidth = 0.5), 
            geom="point",
            #geom_params=list(colour=NA), 
            position = position_identity()
      )  
    
    plot2
  }) 
  
  
  
  observeEvent(input$clicks1, {
    print(as.numeric(input$clicks1))
  })
  
  observeEvent(input$clicks2, {
    print(as.numeric(input$clicks2))
  })
  # Begin code for Second Tab:
  

  
  # Begin code for Third Tab:
  
  df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 """select region || \\\' \\\' || \\\'Sales\\\' as measure_names, sum(sales) as measure_values from SUPERSTORE_SALES_ORDERS
                                                                                 where country_region = \\\'United States of America\\\'
                                                                                 group by region
                                                                                 union all
                                                                                 select market || \\\' \\\' || \\\'Coffee_Sales\\\' as measure_names, sum(coffee_sales) as measure_values from COFFEE_CHAIN
                                                                                 group by market
                                                                                 order by 1;"""
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot3 <- renderPlot(height=1000, width=2000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Blending 2 Data Sources') +
      labs(x=paste("Region Sales"), y=paste("Sum of Sales")) +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES, label=round(MEASURE_VALUES)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      )
    plot3
  })
  
  # Begin code for Fourth Tab:
  output$map <- renderLeaflet({leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17) %>% addPopups(-93.65, 42.0285, 'Here is the Department of Statistics, ISU')
  })
  
  # Begin code for Fifth Tab:
  output$table <- renderDataTable({datatable(df1())
  })
})
