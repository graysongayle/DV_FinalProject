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
  
  df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 """select country_name, X2012-X2002 as calc
                                                                                    from edudata
                                                                                 where  indicator_name = \\\'Children out of school, primary\\\' and X2012-X2002 > 100000
                                                                                 order by calc desc;"""
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gmg954', PASS='orcl_gmg954', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  df4 <- eventReactive(input$clicks4, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 """select country_name, X2012-X2002 as calc
                                                                                 from edudata
                                                                                 where  indicator_name = \\\'Children out of school, primary\\\' and X2012-X2002 < -3000000
                                                                                 order by calc desc;"""
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gmg954', PASS='orcl_gmg954', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
    
  
  output$distPlot3 <- renderPlot(height=450, width=1400, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Number of additional children out of school since 2002 to 2012 ( > 100,000)') +
      labs(x=paste("Country Name"), y=paste("Difference between 2012 and 2002")) +
      layer(data=df3(), 
            mapping=aes(x=COUNTRY_NAME, y=CALC), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour=NA), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df3(), 
            mapping=aes(x=COUNTRY_NAME, y=CALC, label=(CALC)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()) 
    plot3
  })
  

  
  output$distPlot4 <- renderPlot(height=450, width=1400, {
    plot4 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Number of additional children out of school since 2002 to 2012 ( < 3,000,000)') +
      labs(x=paste("Country Name"), y=paste("Difference between 2012 and 2002")) +
      layer(data=df4(), 
            mapping=aes(x=COUNTRY_NAME, y=CALC), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour=NA), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df4(), 
            mapping=aes(x=COUNTRY_NAME, y=CALC, label=(CALC)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=+1.25), 
            position=position_identity()) 
    plot4
  })
  

  observeEvent(input$clicks3, {
    print(as.numeric(input$clicks3))
  })
  
  observeEvent(input$clicks4, {
    print(as.numeric(input$clicks4))
  })
  
  
  # Begin code for Third Tab:
  
  KPI_MidVal <- reactive({input$KPI1})     
  
  rv <- reactiveValues(alpha = 0.50)
  
  
 
  
  df5 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 "  select country_name, indicator_name, kpi as diff,
                                                                                  case
                                                                                  when kpi < "p1" then \\\'03 Low\\\'
                                                                                 
                                                                                 else \\\'01 High\\\'
                                                                                 end kpi
                                                                                  from(
                                                                                 select country_name, X2010-X2000 as kpi, indicator_name
                                                                                 from edudata
                                                                                 where indicator_name like \\\'%Literacy rate, youth%\\\' and X2010-X2000 is not null
                                                                                 order by kpi desc);"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gmg954', PASS='orcl_gmg954', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_MidVal(), verbose = TRUE)))
  )})
  
  
  output$distPlot5 <- renderPlot(height=700, width=1500,{             
    
    
    plot5 <-  ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title=isolate(input$title)) +
      labs(x=paste("INDICATOR_NAME"), y=paste("COUNTRY_NAME")) +
      layer(data=df5(), 
            mapping=aes(x=INDICATOR_NAME, y=COUNTRY_NAME, label=DIFF), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df5(), 
            mapping=aes(x=INDICATOR_NAME, y=COUNTRY_NAME, fill=DIFF), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=rv$alpha), 
            position=position_identity()
      )
    plot5
  })

  
  observeEvent(input$clicks5, {
    print(as.numeric(input$clicks5))
  })
  
  
  
  
  
  
  
  
  
  
 
}
)
