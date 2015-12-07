# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)



df1 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                 """select X2000, X2012
                                                                               from edudata
                                                 where (indicator_name = \\\'Pupil-teacher ratio, primary\\\'
                                                 ) and (X2000 is not null and X2012 is not null)
                                                 order by X2000 desc
                                                 """
                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gmg954', PASS='orcl_gmg954', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); 


plot <- 
  ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  #facet_wrap(~CLARITY, ncol=1) +
  labs(title='Student-Teacher Ratio for Primary Schools from 2000 to 2012') +
  labs(x=paste("Year 2000"), y=paste("Year 2012")) +
  layer(data=df1, 
        mapping=aes(x=X2000, y=X2012), 
        stat="identity", 
        #stat_params=list(binwidth = 0.5), 
        geom="point",
        #geom_params=list(colour=NA), 
        position = position_identity()
  )  


df2 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                 """select X2000, X2012
                                                 from edudata
                                                 where (indicator_name = \\\'Pupil-teacher ratio, secondary\\\'
                                                 ) and (X2000 is not null and X2012 is not null)
                                                 order by X2000 desc
                                                 """
                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_gmg954', PASS='orcl_gmg954', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); 



plot2 <- 
  ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  #facet_wrap(~CLARITY, ncol=1) +
  labs(title='Student-Teacher Ratio for Primary Schools from 2000 to 2012') +
  labs(x=paste("Year 2000"), y=paste("Year 2012")) +
  layer(data=df2, 
        mapping=aes(x=X2000, y=X2012), 
        stat="identity", 
        #stat_params=list(binwidth = 0.5), 
        geom="point",
        #geom_params=list(colour=NA), 
        position = position_identity()
  )  
plot2
