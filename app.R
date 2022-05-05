# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(symengine)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(datasets)
library(ggplot2)
library(plotly)
library(jpeg)
library(tidyverse)
library(viridis)
library(sf)
library(tmap)
library(RColorBrewer)
library(ggpubr)
library(ggrepel)
library(stringr)
library(dplyr)
library(scales)
library(DT)
library(shinyauthr)

data<-read_csv("data.csv")
data_state<-read_csv("data_statenew.csv")
data_ratio<-read_csv("data_long.csv")
data_ratio_state<-read_csv("data_long_state.csv")
data_india<-st_read("data_long.shp")


user_base <- tibble::tibble(
  user = c("school", "college"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)


# Define UI for application that draws a histogram
ui<- fluidPage(
    
  
  
  
  
  # logout button
    
    #theme = shinytheme("simplex"), 
    #  setBackgroundImage(src ="nimr.jpg"),
    
    HTML('<font color="black">'), h1(HTML('<left><img src="scienceicon.jpg" width=100 height=80></left> ', alt="Demo School Dashboard (SDB)"), align="center", HTML('<img src="scienceicon.jpg" width=100 height=80>')), HTML('</font>'),
    
    #HTML('<font color="blue">'),
    sidebarLayout(
        sidebarPanel(
            #width=3,
            
            #_____________________________________________________________________________#
            #                          Tab 0 : Home Page                                  #
            #_____________________________________________________________________________#
            
          
            conditionalPanel(condition="input.tabselected==0",  h2("About the Dashboard", br(), align="center") ,  h5("Demo School Dashboard (SDB) enables quick analysis of School data. It gives interactive graphs and tables as required by the user.", align="justify"),  h5("\n  This dashboard cannot be distributed or shared or altered without the prior permission of the Principal, Hamara School Pyara School. The code for the dashboard is proprietary and cannot be shown or shared with anyone.", align="justify"),br(),
                             
                             div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                             
                             # login section
                             shinyauthr::loginUI(id = "login"),
                             #br(),
                             
                             h4("Dr XYZ  & Dr ABC", align="center"),  h5("Hamara School Pyara School", align="center"), h6("Email : xyz@gmail.com, abc@gmai.com", align="center")),
            
            
            #_____________________________________________________________________________#
            #                             Tab 1 : D1 (Graph-T1)                           #
            #_____________________________________________________________________________#
            
            
            conditionalPanel(condition="input.tabselected==1",
                             
                             
                             #HTML('<center><img src="NIMRNew.jpeg" width=240 height=60></center>'),
                             h5(selectInput( "state", h5(strong("Select the State")), choices = unique(data$state), selected="ASSAM")),
                             h5(selectInput("district", h5(strong("Select the District")), choices="", selected = unique(data$district))),
                             h5(selectInput("variable", h5(strong("Select the Variable")), choices=c("npi", "nfi", "nrt", "npr", "nal", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "npi")),
                             h5(checkboxGroupInput("year", h5(strong("Select the Year")),choices = unique(data$year), selected=data$year), align="left")
                             #h5(sliderInput("year", h5(strong("SELECT THE YEAR")),min = 2002, max = 2019, value = c(2002, 2019)))
                             
                                    
                             
                             
            ),
            
            
            #_____________________________________________________________________________#
            #                             Tab 2 : D2 (Graph-T2)                           #
            #_____________________________________________________________________________#
            
            
            conditionalPanel(condition="input.tabselected==2",
                       
                             
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput( "state2", h5(strong("Select the State")), choices = unique(data$state), selected = "ASSAM")),
                             h5(selectInput("year2", h5(strong("Select the year")), choices=unique(data$year), selected = "2000")),
                             h5(selectInput("variable2", h5(strong("Select the Variable")), choices=c("npi", "nfi", "nrt", "npr", "nal", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "npi"))
                             
            ),
            
            #_____________________________________________________________________________#
            #                             Tab 3 : D3 (Graph-T2)                           #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==3",
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput( "state3", h5(strong("Select one or more State")), choices = unique(data$state), selected="ASSAM", multiple=TRUE)),
                             h5(selectInput("district3", h5(strong("Select district one or more")), choices="", multiple=TRUE,selected = unique(data$district))),
                             h5(selectInput("variable3", h5(strong("Select a Variable")), choices=c("npi", "nfi", "nrt", "npr", "nal", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "npi")),
                             h5(selectInput("year31", h5(strong("Select the starting year")), choices=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,2016,2017,2018,2019), selected = 2000)),
                             h5(selectInput("year32", h5(strong("Select the ending year")), choices=c(2000, 2001,2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,2016,2017,2018,2019), selected = 2019)),
                             h5(radioButtons("lab3", h5(strong("Labels")), choices=c("On", "Off"), selected = "Off"))
                             
                             
            ),
            
            #_____________________________________________________________________________#
            #                             Tab 4 : D4 (Graph)                              #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==4",
                             # HTML('<center><img src="icmr4.jpg" width=380 height=200></center>'),
                             h5(selectInput( "state4", h5(strong("Select the State")), choices = unique(data$state), selected="ASSAM")),
                             h5(selectInput("year4", h5(strong("Select the year")), choices=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,2016,2017,2018,2019), selected = 2000)),
                             h5(selectInput("variable4x", h5(strong("Select the X Variable")), choices=c("npi", "nfi", "nrt", "npr", "nal", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "npi")),
                             h5(selectInput("variable4y", h5(strong("Select the Y Variable")), choices=c("npi", "nfi", "nrt", "npr", "nal", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "npr")),
                             h5(numericInput("xcenter", "Enter X Center", value=0, step=.2)),
                             h5(numericInput("ycenter", "Enter Y Center", value=0, step=.2)),
                             h5(radioButtons("lab4", h5(strong("Labels")), choices=c("On", "Off"), selected = "Off"))
                             
                             
            ),
            
            
            #_____________________________________________________________________________#
            #                             Tab 5 : D5 (Graph)                                      #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==5",
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput( "state5", h5(strong("Select the State")), choices = unique(data$state), selected="ASSAM")),
                             h5(selectInput("year5", h5(strong("Select one or more year")), choices=unique(data$year),multiple=TRUE, selected = 2000)),
                             h5(radioButtons("lab5", h5(strong("Labels")), choices=c("On", "Off"), selected = "Off"))
                             
                             
                             #h5(selectInput("variable10x", h5(strong("Select the X Variable")), choices=c("api", "aber", "spr",  "sfr", "pv", "pf", "total", "pf_per", "pv_per"), selected = "api")),
                             #h5(selectInput("variable10y", h5(strong("Select the Y Variable")), choices=c("api", "aber", "spr",  "sfr", "pv", "pf", "total", "pf_per", "pv_per"), selected = "spr")),
                             #h5(numericInput("xcenter", "Enter X Center", value=0, step=.2)),
                             #h5(numericInput("ycenter", "Enter Y Center", value=0, step=.2))
                             
                             
                             
            ),
            
            
            
            
            #_____________________________________________________________________________#
            #                             Tab 6 : D4 (Data)                               #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==6",
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput( "state6", h5(strong("Select one or more State")), choices = unique(data$state), selected="ASSAM", multiple=TRUE)),
                             h5(selectInput("district6", h5(strong("Select one or more district")), multiple=TRUE, choices="", selected = unique(data$district))),
                             h5(checkboxGroupInput("year6", h5(strong("Select the year")),choices = unique(data$year), selected=data$year), align="left")
                             
            ),
            
            
            
            
            #_____________________________________________________________________________#
            #                             Tab 7 : S1 (Graph-T1)                           #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==7",
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput( "state7", h5(strong("Select the State")), choices = unique(data_state$state), selected="ASSAM")),
                             h5(selectInput("variable7", h5(strong("Select the Variable")), choices=c("npi", "nfi", "nrt", "npr", "nal", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "api"),
                                h5(checkboxGroupInput("year7", h5(strong("Select the Year")),choices = unique(data_state$year), selected=data_state$year), align="left"))
                             
                             
                             
            ),
            
            #_____________________________________________________________________________#
            #                             Tab 8 : S2 (Graph-T2)                           #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==8",
                             
                             
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput("year8", h5(strong("Select the year")), choices=unique(data_state$year), selected = "2000")),
                             h5(selectInput("variable8", h5(strong("Select the Variable")), choices=c("npi", "nfi", "nrt", "npr", "ner", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "api")),
                             h5(selectInput("style8", h5(strong("SELECT THE STYLE (for map only)")),choices = c("pretty", "equal", "quantile", "jenks", "cont", "hclust", "fisher", "sd", "bclust")), align="left"),
                             h5(selectInput("colstyle8", h5(strong("SELECT THE COLOUR STYLE (for map only)")),choices = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu")), align="left")
                             
                             
                             
            ),
            
            #_____________________________________________________________________________#
            #                             Tab 9 : S3 (Graph-T3)                           #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==9",
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             # h5(selectInput("year71", h5(strong("Select the First year")), choices=c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015), selected = 2002)),
                             #h5(selectInput("year72", h5(strong("Select the Second year")), choices=c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015), selected = 2003)),
                             h5(selectInput( "state9", h5(strong("SELECT THE STATE")), choices = unique(data_state$state), multiple=TRUE)),
                             h5(selectInput("variable9", h5(strong("Select the Variable")), choices=c("npi", "nfi", "nrt", "npr", "ner", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "api")),
                             h5(checkboxGroupInput("year9", h5(strong("SELECT THE YEAR")),choices = unique(data_state$year), selected=data_state$year), align="left")
                             
                             
                             
                             
            ),
            
            
            
            
            
            
            #_____________________________________________________________________________#
            #                             Tab 10 : S4 (Data)                               #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==10",
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput( "state10", h5(strong("Select the State")), choices = unique(data_state$state), selected="ASSAM")),
                             h5(checkboxGroupInput("year10", h5(strong("Select the State")),choices = unique(data_state$year), selected=data_state$year), align="left")
                             
            ),
            
            #_____________________________________________________________________________#
            #                             Tab 11 : S5                                      #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==11",
                             #HTML('<center><img src="icmr4.jpg" width=380 height=200></center>'),
                             #h5(selectInput("variable11", h5(strong("Select the Variable")), choices=c("api", "aber", "spr",  "sfr", "pv", "pf", "total", "pf_per", "pv_per"), selected = "api")),
                             #h5(checkboxGroupInput("year11", h5(strong("SELECT THE YEAR")),choices = unique(data_country$year), selected=data_country$year), align="left")
                             h5(selectInput("year11", h5(strong("Select the one or more year")), choices=unique(data_ratio_state$year),multiple=TRUE, selected = 2000)),
                             h5(radioButtons("lab11", h5(strong("Labels")), choices=c("On", "Off"), selected = "Off"))
                             
                             
                             
                             
            ),
            
            
            #_____________________________________________________________________________#
            #                             Tab 12 : S6                                      #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==12",
                             #HTML('<center><img src="icmr4.jpg" width=380 height=200></center>'),
                             #h5(selectInput("variable11", h5(strong("Select the Variable")), choices=c("api", "aber", "spr",  "sfr", "pv", "pf", "total", "pf_per", "pv_per"), selected = "api")),
                             #h5(checkboxGroupInput("year11", h5(strong("Select one or more years")),choices = unique(data_country$year), selected=data_country$year), align="left")
            ),
            
            
            #_____________________________________________________________________________#
            #                             Tab 13 : C1                                      #
            #_____________________________________________________________________________#
            
            conditionalPanel(condition="input.tabselected==13",
                             # HTML('<center><img src="icmr4.jpg" width=380 height=200></center>'),
                             # h5(selectInput("variable11", h5(strong("Select the Variable")), choices=c("api", "aber", "spr",  "sfr", "pv", "pf", "total", "pf_per", "pv_per"), selected = "api")),
                             # h5(checkboxGroupInput("year11", h5(strong("SELECT THE YEAR")),choices = unique(data_country$year), selected=data_country$year), align="left")
                             # 
                             
                             
                             #HTML('<center><img src="NIMRNew.jpeg" width=380 height=120></center>'),
                             h5(selectInput( "year13", h5(strong("Select one or more years")), choices = unique(data_india$year_), multiple=TRUE, selected="2002")),
                             h5(selectInput( "state13", h5(strong("Select the State")), choices = unique(data_india$STATENAME), selected = "Assam",  multiple=TRUE)),
                             h5(selectInput("variable13", h5(strong("Select the Variable")), choices=c("npi", "nfi", "nrt", "npr", "nal", "nfr", "npv", "npf", "nal", "ner", "nse"), selected = "npi")),
                             
                             
                             h5(radioButtons("style13", h5(strong("Select the style")),choices = c("pretty", "equal", "quantile", "jenks", "cont", "hclust", "fisher", "sd", "bclust")), align="left"),
                             h5(radioButtons("colstyle13", h5(strong("Select the coloure scheme")),choices = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu")), align="left")
                             
                             
            ),
            
            
            
            
            
            
            tags$style("body{background-color:black; color:black}")
            
            
        ),  
        
        
        #__________________________________________________________________________________#
        #__________________________________MAIN PANEL______________________________________#
        #__________________________________________________________________________________#    
        
        
        mainPanel( 
            #setBackgroundColor("darkolivegreen1"),  
            #textInput(inputId="text", label = "Text"), textInput(inputId="text", label = "Text"),
            
          
          
          
            setBackgroundColor(
                color =  c("#F7FBFF", "#2171B5"),
                gradient = "linear",
                direction = "bottom"
            ),

      
            
            
                  #   setBackgroundColor(
            #   color = c("darkorchid", "darkorchid1"),
            #   gradient = "linear",
            #   direction = "bottom"
            # ),

            
            
            # setBackgroundColor(color = "ghostwhite", gradient = c("linear", "radial"),
            #                    direction = c("bottom", "top", "right", "left")),
            # 
            #setBackgroundColor("ghostwhite"),
            
            # setBackgroundColor(
            #   color = c("#F7FBFF", "#2171B5"),
            #   gradient = "radial",
            #   direction = c("top", "left")
            #),
            
            
        
            # h6("Developed by Dr. C.P. Yadav", aligh="center"),
            
            
            HTML('<font color="navy">'),
            tabsetPanel(type="tab", 
                        tabPanel(h4(strong("Home", align="center")), value=0,icon = icon("institution", "fa-1.9x"), plotOutput("nimr")),
                        
                        tabPanel(h4(strong("D1"  , align="left")), value=1,icon = icon("line-chart", "fa-1.9x"), plotOutput("t1_g", height = "450px"), br(), plotOutput("t1_g2")),
                                 
                  
                        
                        #,  downloadButton("download_state", "Download")
                        
    
                        
                        
                        tabPanel(h4(strong("D2"  , align="left")), value=2,icon = icon("bar-chart", "fa-1.9x") , plotOutput("t2_g1",height = "600px", width="100%"), plotOutput("t2_g2", height = "600px", width="100%"), plotOutput("t2_g3", height = "600px", width="100%")),
                        tabPanel(h4(strong("D3"  , align="left")), value=3,icon = icon("align-center", "fa-1.9x"), plotOutput("t3_g"), br(),br(),br(),br(),br(),br(),br(),br(),plotOutput("t3_g2",height = "600px")  ),
                        tabPanel(h4(strong("D4"  , align="center")), value=4,icon = icon("braille", "fa-1.9x"), plotOutput("t4_g"), plotOutput("t4_g2")), 
                        tabPanel(h4(strong("D5"  , align="center")), value=5,icon = icon("signal", "fa-1.9x"), plotOutput("t5_g", width="130%", height="700px")), 
                        tabPanel(h4(strong("D6"  , align="left")), value=6,icon = icon("database", "fa-1.9x") , DT::dataTableOutput("dataset_district")), 
                        
                        tabPanel(h4(strong("S1"  , align="center")), value=7,icon = icon("line-chart", "fa-1.9x") , plotOutput("t7_g"), plotOutput("t7_g2", width = "120%",  height = "600px")),
                        tabPanel(h4(strong("S2"  , align="center")), value=8,icon = icon("bar-chart", "fa-1.9x"),plotOutput("t8_g", height = "680px", width="99%"),plotOutput("t8_g3", height = "680px", width="99%"),  plotOutput("t8_g4",  width = "140%",  height = "800px"), plotOutput("t8_g2",  width = "99%",  height = "600px")), 
                        tabPanel(h4(strong("S3"  , align="center")), value=9,icon = icon("align-center", "fa-1.9x"), plotOutput("t9_g"), plotOutput("t9_g2")),  
                        tabPanel(h4(strong("S4"  , align="center")), value=10,icon = icon("braille", "fa-1.9x"),DT::dataTableOutput("dataset_state")),
                        tabPanel(h4(strong("S5"  , align="center")), value=11,icon = icon("signal", "fa-1.9x"),plotOutput("t11_g", width="130%", height="700px") ),
                        tabPanel(h4(strong("S6"  , align="center")), value=12,icon = icon("database", "fa-1.9x")),
                        
                        
                        
                        
                        tabPanel(h4(strong("C1"  , align="center")), value=13,icon = icon("certificate", "fa-1.9x"), plotOutput("t13_g", width="130%", height="500px"), plotOutput("t11_g2", width="130%", height="500px"), plotOutput("t11_g3", width="3000%", height="500px")),
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        #tabPanel(h4(strong("State Data11", align="center")), value=10 ,icon = icon("database", "fa-3x"))
                        
                        #tabPanel(h4(strong("Death", align="center")), value=3, tableOutput("death")     ,icon = icon("bed", "fa-3x")                ,downloadButton("download_death", "Download")),
                        #tabPanel(h4(strong("Data" , align="center")), value=4, DT::dataTableOutput("dataset")  ,icon = icon("table", "fa-3x")      ,downloadButton("download_data" , "Download")),
                        # tabPanel(h4(strong("State Data", align="center")), value=5, verbatimTextOutput("statedata"),icon = icon("database", "fa-3x")   ,downloadButton("download_state", "Download")),
                        # tabPanel(h4(strong("Data11" , align="center")), value=6, DT::dataTableOutput("dataset33")  ,icon = icon("table", "fa-3x")      ,downloadButton("download_data44" , "Download")),
                        # #,
                        id="tabselected",
                        
                        
                        
                        h5("Data Source : Hamara School Pyara Schol", align = "right"),
                        h4("Developed by Dr. XYZ", align = "center"),
                        h5("Teacher", align = "center"),
                        h6("Hamara School Pyara Schol, New Delhi", align = "center")
                        
                        
            )
        )
    )
)

# Define server logic required to draw a histogram

server<-function(session, input, output){
    
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
    
    my_image=readJPEG("scienceicon1.jpg")
    
    
    var<-reactive({
            
        names(data$variable)
        
    })
    
    
    
    ############################ for Tab 1
    observeEvent(input$state,
                 
                 
                              updateSelectInput(session, "district", "Select the Districts", choices = data$district[data$state==input$state])
                 
    )
    
    datareact<-reactive({
        
        data[data$state==input$state & data$district==input$district  & data$year %in% input$year,]
        
    })
    
    ############################ for Tab 2
    datareact2<-reactive({
        
        data[data$state==input$state2 & data$year== input$year2,]
        
    })
    
    
    ############################ for Tab 3
    observeEvent(input$state3,
                 updateSelectInput(session, "district3", "Select one or more Districts", choices = data$district[data$state==input$state3])
                 
    )
    
    datareact3<-reactive({
        
        data[data$state==input$state2 ,]
        
    })
    
    ############################ for Tab 4
    
    reactive4<-reactive({
        data[data$year == input$year4 & data$state ==input$state4,]
        
    })
    
    
    ############################ for Tab 5
    
    
    reactive5<-reactive({
        data_ratio[data_ratio$state == input$state5 & data_ratio$year %in% input$year5,]
        
        
    })
    
    
    
    ############################ for Tab 6
    
    ### for Tab 6
    observeEvent(input$state6,
                 updateSelectInput(session, "district6", "Select one or more districts", choices = data$district[data$state==input$state6])
                 
    )
    
    
    ############################ for Tab 7
    
    datareact7<-reactive({
        data_state[data_state$state==input$state7  & data_state$year %in% input$year7,]
        
    })
    
    datareact7new<-reactive({
        data_assam[data_assam$year %in% input$year7,]
        
    })
    
    ############################ for Tab 8
    
    reactiv8<-reactive({
        
        data_state[data_state$year==input$year8,]
        
    })
    
    
    
    ############################ for Tab 9
    
    reactive9<-reactive({
        data_state[data_state$state %in% input$state9 & data_state$year %in% input$year9,]
        
        
    })
    
    
    #_____________________Data set for District
    
    datareact_data_district<-reactive({
      
        data[data$state==input$state6 & data$district %in% input$district6  & data$year %in% input$year6,]
        
    })
    
    
    #_____________________Data set for State
    
    datareact_data_state<-reactive({
        
        data_state[data_state$state==input$state10   & data_state$year %in% input$year10,]
    
          
    })
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Home Tab________________________________________#
    #__________________________________________________________________________________#    
    
    output$nimr<-renderPlot({
      req(credentials()$user_auth)  
        plot(1:2, type='n', main="WELCOME", xlab="Hamara School Pyara Schol, New Delhi", ylab="", xaxt="n",yaxt="n",  cex.lab = 1.4, cex.main=3)
        lim <- par()
        rasterImage(my_image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

      
              
    })
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-1___________________________________________#
    #__________________________________________________________________________________#    
    
    
    
    output$t1_g<-renderPlot({
      req(credentials()$user_auth)  
        
        palette(c("#E41A1C", "#377EB8", "#4DAF4A",
                  "#984EA3", "#FF7F00", "#FFFF33",
                  "#A65628", "#F781BF", "#999999"))
        
        
        par(mar=c(5, 5, 5, 1), xpd=TRUE)
        
        with(datareact(), plot(year, get(input$variable),  type="b", lwd=3, col="violetred1",  ylab=toupper(input$variable), xlab="Year",main= str_to_title( input$district),  cex.main=2, col.main="black", cex.axis = 1.2, cex.lab = 1.5,lty=1, frame.plot=FALSE, xaxt="n",yaxt="n",xpd=TRUE, col.lab='black' ))
        with(datareact(), text(year, get(input$variable), get(input$variable), cex=1, col="black", font=2, pos=3,  xpd=TRUE ))
        axis(1, at=seq(min(datareact()$year, na.rm=TRUE), max(datareact()$year, na.rm=TRUE), by=1) , cex.axis=1.0,  lwd=1.5, col="black",  col.axis="black")
        with(datareact(), axis(2, at=round(get(input$variable)[state==input$state & district==input$district  & year %in% input$year],2) ,cex.axis=1.0,  lwd=1.5, col="black", col.axis="black"))
        
        
        #axis(2, at=seq(0, max(datareact()$api , na.rm=TRUE), by=quantile(datareact()$api, 0.10, na.rm = TRUE)) , cex.axis=1.0,  lwd=1.5, col="royalblue",  col.axis="royalblue")
        #with(datareact(), axis(2, at=seq(min(datareact()$api, na.rm=TRUE), max(datareact()$api, na.rm=TRUE)) ,cex.axis=1.0,  lwd=1.5, col="royalblue", col.axis="royalblue", col.lab='royalblue'))
        
        # my_image=readJPEG("nimr.jpg")
        # plot(1:2, type='n', main="", xlab="x", ylab="y")
        # lim <- par()
        # rasterImage(my_image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
        # grid()
        
        
        
        # output$t1_g2<-renderPlot({
        #   
        #   
        #   ggplot(datareact(), aes(x=year, y=get(input$variable)))+ geom_line(col="deeppink", size=1) + geom_smooth(col="black", size=1.5, fill="gold1")+theme_classic()+theme(axis.text = element_text(size=14), plot.title=element_text(size=30, face="bold", hjust=.5), axis.title=element_text(size=20))+labs(y=input$variable, x="Year", title="")+
        #     scale_x_continuous(breaks = breaks_width(1))
        #   
        #   #
        #   # ggplot(reactiv5(), aes(x = reorder(state,get(input$variable5)))) +
        #   #   geom_col(aes(y = get(input$variable5)), fill = "chocolate1", alpha=.7) +coord_flip()+theme_classic()+
        #   #   geom_text(aes(y=get(input$variable5), hjust=-0.01, label=round(get(input$variable5),2)))+labs(x="state", y=input$variable5)+
        #   #   theme(axis.title = element_text(size=20), axis.text = element_text(size=12, family = 8))
        #   # 
        #   
        #   
        # })
        # 
        
    })
    
    datareact11new<-reactive({
        
        data[data$state==input$state & data$year %in% input$year,]
        
    })
    
    
    output$t1_g2<-renderPlot({

      
            req(credentials()$user_auth)  
        ggplot(datareact11new(), aes(factor(year), get(input$variable), col=factor(year)))+ 
            geom_boxplot() + geom_jitter()+
            geom_smooth(method = "lm", se=FALSE, color="black")+theme_classic()+
            stat_summary(fun.y=median, colour="red", aes(group=1),
                         geom="line", lwd=1.5, lty=1)+labs(title="PV Trend in ANDHRA PRADESH", x= "Year", y="PV Cases")+theme(legend.position = "none", axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
            stat_summary(fun.y=mean, colour="green", aes(group=1),
                         geom="line", lwd=1.5, lty=1)+labs(title="PV Trend in ANDHRA PRADESH", x= "Year", y="PV Cases")+theme(legend.position = "none", axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
            geom_smooth(method = "glm",aes(group=1),  se = F)+
            labs(x="Year", y=toupper(input$variable), title=paste( "State : ", str_to_title(input$state)))+
            theme(axis.text = element_text(size=14), plot.title=element_text(size=30, face="bold", hjust=.5), axis.title=element_text(size=20))
        
    })
    
    #__________________________________________________________________________________#
    #__________________________________Tab-2___________________________________________#
    #__________________________________________________________________________________#    
    
    
    #:::::::::::::::::::::Graph-1
    
    
    output$t2_g1<-renderPlot({
      req(credentials()$user_auth)  
      
        ggplot(datareact2(), aes(x = reorder(district,get(input$variable2)))) +
            geom_col(aes(y = get(input$variable2)), fill = "chocolate1", alpha=.7) +coord_flip()+theme_classic()+
            geom_text(aes(y=get(input$variable2), hjust=-0.01,fontface="bold", label=round(get(input$variable2),2)))+labs(x="District", y=toupper(input$variable2), title=paste( "Districts of ", str_to_title(input$state2), "on", toupper(input$variable2), "for the year :", input$year2))+
            theme(axis.title = element_text(size=20),plot.title = element_text(size=25, hjust=.5, face="bold"), axis.text = element_text(size=12, family = 8))
        
    })
    
    
    
    #:::::::::::::::::::::Graph-2
    
    
    
    datareact2new<-reactive({
      req(credentials()$user_auth)  
      
        data_assam[data_assam$year== input$year2,]
        
    })
    
    
    output$t2_g2<-renderPlot({
      req(credentials()$user_auth)  
      
        
        
        ggplot(datareact3(), aes(x=district, y=get(input$variable2), fill=district)) +geom_boxplot()+theme_classic()+theme(legend.position ="none", plot.title = element_text(size=25, hjust=.5))+coord_flip()+labs(y=toupper(input$variable2), x="District",title =paste( "Districts of ", str_to_title(input$state2), "on", toupper(input$variable2), "over the years"))+theme(axis.title = element_text(size=20,face="bold"), axis.text = element_text(size=12, family = 8))
        
        
    })
    
    
    
    output$t2_g3<-renderPlot({
      req(credentials()$user_auth)  
      
        tm_shape(datareact2new()) +
            tm_borders() +
            #tm_facets(by=input$year2) +
            tm_fill(input$variable2, style="quantile") +
            tm_shape(datareact2new()) +
            tm_borders(lwd=2)+
            tm_layout(
                main.title = paste( "GIS Mapping of ", str_to_title(input$state2), "on", toupper(input$variable2),  "for the year :", input$year2), main.title.position = "center")
        
        
        
        
        
        #+
        #tm_facets(by="year", nrow=4, ncol=4,  free.coords = FALSE)+
        #tm_layout(panel.label.size = 2.5)
        
        
    })
    
    
    
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-3___________________________________________#
    #__________________________________________________________________________________#    
    
    reactive3<-reactive({
      
      req(credentials()$user_auth)  
      
        data[data$state == input$state3 & data$district %in% input$district3 & (data$year >= input$year31 & data$year <= input$year32),]
        
    })
    
    output$t3_g<-renderPlot({
        
      req(credentials()$user_auth)  
      
        if(input$lab3=="Off") {
            
            ggplot(reactive3(), aes(x = year, y = get(input$variable3), group = district)) +
                geom_line(aes(color =    district), size = 1.2) +
                geom_point(color ="black", size = 1.5) + theme_classic()+
                #geom_label_repel(aes(y=get(input$variable3), hjust=-0.01,fontface="bold", label=round(get(input$variable3),2)))+
                labs(title = paste("Districts comparison on the basis of",toupper(input$variable3) ), x="Year", y=toupper(input$variable3))+
                scale_x_continuous(breaks = breaks_width(1))+
                theme(plot.title = element_text(size=20, hjust=.5, face="bold"), axis.title = element_text(size=20), axis.text = element_text(size=15))+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            
        }else{
      
          req(credentials()$user_auth)  
          
            ggplot(reactive3(), aes(x = year, y = get(input$variable3), group = district)) +
                geom_line(aes(color =    district), size = 1.2) +
                geom_point(color ="black", size = 1.5) + theme_classic()+
                geom_text(aes(y=get(input$variable3), hjust=-0.01,fontface="bold", label=round(get(input$variable3),2)))+
                labs(title = paste("Districts comparison on the basis of",toupper(input$variable3)), x="Year", y=toupper(input$variable3))+
                scale_x_continuous(breaks = breaks_width(1))+
                theme(plot.title = element_text(size=20, hjust=.5, face="bold"), axis.title = element_text(size=20), axis.text = element_text(size=15))+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            
            
        }
        
        
        
        
    })
    
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #   
    #   
    #   View(data_wide)
    
    reactive32<-reactive({
        
        
        #var1<-paste(input$variable3,input$year31,sep="")
        #var2<-paste(input$variable3,input$year32,sep="")
        
        data_wide$difference<-((data_wide[,paste(input$variable3,input$year32,sep="")]-data_wide[,paste(input$variable3,input$year31,sep="")])*100)/data_wide[,paste(input$variable3,input$year31,sep="")]
        
        
        data_wide$type<-ifelse(data_wide$difference < 0, "Decrease", "Increase")
        
        
        data_wide<-data_wide[order(data_wide$difference),]
        data_wide[data_wide$state == input$state3 & data_wide$district %in% input$district3 ,]
        
        
        
        
        
    })
    
    output$t3_g2<-renderPlot({
      req(credentials()$user_auth)  
      
        ggplot(reactive32(), aes(x=district, y=difference,label=difference)) +
            geom_bar(stat='identity', aes(fill=type), width=.5) +
            scale_fill_manual(name="difference", values = c("Increase"="#f8766d", "Decrease"="#00ba38"))+
            coord_flip()  +
            geom_text(aes(y=difference, hjust=-0.01,fontface="bold", label=round(difference,2)))+
            labs(x="District", y="Percentage", title=paste("Percentage change from", input$year31, "to ",input$year32))+theme_classic()+
            theme(axis.title = element_text(size=15, color="black", face="bold"),plot.title = element_text(size=30, hjust=.5, face="bold", color="black"), axis.text = element_text(size=12, family = 8, face="bold", colour="black"))
        
    })
    
    
    
    
    
    # + 
    #   
    #   
    #   
    #   geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
    #   scale_fill_manual(name="Mileage", 
    #                     labels = c("Above Average", "Below Average"), 
    #                     values = c("above"="#00ba38", "below"="#f8766d")) + 
    #   labs(subtitle="Normalised mileage from 'mtcars'", 
    #        title= "Diverging Bars") + 
    #   coord_flip()
    # 
    # 
    # 
    # 
    # 
    # 
    #   
    # library(ggplot2)
    # theme_set(theme_bw())  
    # 
    # rm(list=ls())
    # # Data Prep
    # data("mtcars")  # load data
    # View(mtcars)
    # 
    # mtcars$`car name` <- rownames(mtcars)  # create new column for car names
    # mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
    # mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
    # mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
    # mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.
    # 
    # # Diverging Barcharts
    # ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
    #   
    #   
    #   
    #   geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
    #   scale_fill_manual(name="Mileage", 
    #                     labels = c("Above Average", "Below Average"), 
    #                     values = c("above"="#00ba38", "below"="#f8766d")) + 
    #   labs(subtitle="Normalised mileage from 'mtcars'", 
    #        title= "Diverging Bars") + 
    #   coord_flip()
    # 
    # 
    # 
    # 
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-4___________________________________________#
    #__________________________________________________________________________________#    
    
    
    
    # reactive4new<-reactive({
    #   #data_state[data_state$state %in% input$state8 & data_state$year %in% input$year8,]
    #   data[data$year == input$year4 ,]
    #   
    # })
    # 
    
    # output$t4_g<-renderPlot({
    #   
    #   ggplot(reactive4(), aes(x=get(input$variable4x), y=get(input$variable4y))) + geom_point() +geom_line()+
    #     labs(x=input$variable4x, y=input$variable4y)+geom_text(label=reactive4()$district)+theme_classic()+
    #     theme(plot.title = element_text(size=30, hjust=.5, face="bold"), axis.title = element_text(size=20), axis.text = element_text(size=15))
    #   
    #   
    # })
    
    
    
    output$t4_g<-renderPlot({
      req(credentials()$user_auth)  
      
        if(input$lab4=="On") {
            #x_mid <- mean(c(max(reactive4new()$input$variable4x, na.rm = TRUE), min(reactive4new()$input$variable4x, na.rm = TRUE)))
            
            
            #y_mid <- mean(c(max(reactive4new()$input$variable4y, na.rm = TRUE), min(reactive4new()$input$variable4y, na.rm = TRUE)))
            
            
            #x_mid <- mean(reactive4new()$input$variable4x, na.rm = TRUE)
            
            
            #      y_mid <-mean(reactive4new()$input$variable4y, na.rm = TRUE)
            
            
            
            
            
            reactive4() %>% 
                mutate(quadrant = case_when(get(input$variable4x) > input$xcenter & get(input$variable4y) > input$ycenter   ~ "Q1",
                                            get(input$variable4x) <= input$xcenter & get(input$variable4y) > input$ycenter  ~ "Q2",
                                            get(input$variable4x) <= input$xcenter & get(input$variable4y) <= input$ycenter ~ "Q3",
                                            TRUE                                         ~ "Q4")) %>% 
                ggplot(aes(x = get(input$variable4x), y = get(input$variable4y), color = quadrant)) +
                geom_vline(xintercept = input$xcenter, size=.75, col="blue") + # plot vertical line
                geom_hline(yintercept = input$ycenter, size=.75, col="blue") + # plot horizontal line
                geom_point(size=2) +theme_classic()+geom_label_repel(label=reactive4()$district)+theme_classic()+labs(x=toupper(input$variable4x), y=toupper(input$variable4y))+
                theme(plot.title = element_text(size=30, hjust=.5, face="bold"), axis.title = element_text(size=20), axis.text = element_text(size=18))
            
        }else{
            
            
            reactive4() %>% 
                mutate(quadrant = case_when(get(input$variable4x) > input$xcenter & get(input$variable4y) > input$ycenter   ~ "Q1",
                                            get(input$variable4x) <= input$xcenter & get(input$variable4y) > input$ycenter  ~ "Q2",
                                            get(input$variable4x) <= input$xcenter & get(input$variable4y) <= input$ycenter ~ "Q3",
                                            TRUE                                         ~ "Q4")) %>% 
                ggplot(aes(x = get(input$variable4x), y = get(input$variable4y), color = quadrant)) +
                geom_vline(xintercept = input$xcenter, size=.75, col="blue") + # plot vertical line
                geom_hline(yintercept = input$ycenter, size=.75, col="blue") + # plot horizontal line
                geom_point(size=2) +theme_classic()+theme_classic()+labs(x=toupper(input$variable4x), y=toupper(input$variable4y))+
                theme(plot.title = element_text(size=30, hjust=.5, face="bold"), axis.title = element_text(size=20), axis.text = element_text(size=18))
            
            
        }
        
        
    })  
    
    
    
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-5___________________________________________#
    #__________________________________________________________________________________#    
    
    output$t5_g<-renderPlot({
      req(credentials()$user_auth)  
      
        if(input$lab5=="Off") {
            
            ggplot(reactive5(), aes(x = district, y = freq, group = group, fill = group)) +
                geom_bar(stat = "identity", width = 0.75) +
                coord_flip() +facet_grid (~reactive5()$year)+theme_classic()+
                theme(panel.spacing = unit(1, "lines"), strip.text.x=element_text(size=15, colour="black", face="bold"), plot.title = element_text(size=30, hjust=.5, face="bold"), strip.background = element_rect(colour = "black", fill = "seagreen1"), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.text = element_text(size=18),legend.title  = element_text(size=20))+
                scale_fill_manual(values=c("chartreuse", "gray"))+labs(x="Districts", y="Numbers")+
                # geom_text(label=abs(reactive5()$freq), face="bold", col="blue4", size=4)+
                scale_y_continuous(breaks = pretty(reactive5()$freq),labels = abs(pretty(reactive5()$freq)))+labs(title = "Boys-Girls Distribution")
            
        }else{
            
            ggplot(reactive5(), aes(x = district, y = freq, group = group, fill = group)) +
                geom_bar(stat = "identity", width = 0.75) +
                coord_flip() +facet_grid (~reactive5()$year)+theme_classic()+
                theme(panel.spacing = unit(1, "lines"), strip.text.x=element_text(size=15, colour="black", face="bold"), plot.title = element_text(size=30, hjust=.5, face="bold"), strip.background = element_rect(colour = "black", fill = "seagreen1"), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.text = element_text(size=18),legend.title  = element_text(size=20))+
                scale_fill_manual(values=c("chartreuse", "gray"))+labs(x="Districts", y="Numbers")+
                geom_label_repel(label=abs(reactive5()$freq), fontface = "bold", size=3)+
                scale_y_continuous(breaks = pretty(reactive5()$freq),labels = abs(pretty(reactive5()$freq)))+labs(title = "Boys-Girls Distribution")
            
            
            
        }
        
    })
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-6___________________________________________#
    #__________________________________________________________________________________#    
    
    # output$dataset_district<-renderDataTable({
    #   req(credentials()$user_auth)
    #     datareact_data_district()
    #     
    # })
    # 
    # output$download_data<-downloadHandler(
    #     filename = function(){
    #         
    #         paste("dataset", "csv", sep=".")
    #         
    #     },
    #     
    #     content=function(file){
    #         
    #       req(credentials()$user_auth)
    #       
    #         write.csv(datareact(),file)
    #         
    #     }
    #     
    # )
    # 
    
    # output$download_data<-downloadHandler(
    # 
    #   filename = function(){
    # 
    #     paste("dataset", "csv", sep=".")
    # 
    #   },
    # 
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-7___________________________________________#
    #__________________________________________________________________________________#    
    
    
    
    
    
    
    output$t7_g<-renderPlot({
      req(credentials()$user_auth)  
      
        
        par(mar=c(7, 7, 7, 7), xpd=TRUE)
        
        with(datareact7(), plot(year, get(input$variable7), type="b", lwd=3, col="violetred1",  ylab=toupper(input$variable7), xlab="Year",main= str_to_title(input$state7),  cex.main=2, col.main="black", cex.axis = 1.2, cex.lab = 1.7,lty=1, frame.plot=FALSE, xaxt="n",yaxt="n",xpd=TRUE, col.lab='black' ))
        with(datareact7(), text(year, get(input$variable7), round(get(input$variable7),2), cex=1.2, col="black", font=2, pos=3,  xpd=TRUE ))
        axis(1, at=seq(min(datareact7()$year, na.rm=TRUE), max(datareact7()$year, na.rm=TRUE), by=1) , cex.axis=1.0,  lwd=1.7, col="black",  col.axis="black")
        with(datareact7(), axis(2, at=round(get(input$variable7)[state==input$state7  & year %in% input$year7],2) ,cex.axis=1.0,  lwd=1.7, col="black", col.axis="black"))
        
        
        # with(datareact4(), plot(year, get(input$variable),  type="b", lwd=3, col="violetred1",  ylab=input$variable, xlab="YEAR",main= paste("District :",input$district, "(", input$variable, ")"),  cex.main=2, col.main="black", cex.axis = 1.2, cex.lab = 1.7,lty=1, frame.plot=FALSE, xaxt="n",yaxt="n",xpd=TRUE, col.lab='black' ))
        # with(datareact4(), text(year, get(input$variable), get(input$variable), cex=1.2, col="black", font=2, pos=3,  xpd=TRUE ))
        # axis(1, at=seq(min(datareact4()$year, na.rm=TRUE), max(datareact()$year, na.rm=TRUE), by=1) , cex.axis=1.0,  lwd=1.7, col="black",  col.axis="black")
        # with(datareact4(), axis(2, at=get(input$variable)[state==input$state & district==input$district  & year %in% input$year] ,cex.axis=1.0,  lwd=1.7, col="black", col.axis="black"))
        
        
        # par(mar=c(7, 7, 7, 7), xpd=TRUE)
        # 
        # with(datareact_state(), plot(year[state==input$state_state & year %in% input$year_state], api[state==input$state_state & year %in% input$year_state], type="o", lwd=3, col="deeppink",  ylab="API", xlab="YEAR",main= paste("Annual Parasite Index (API) Over the Years : ",input$state_state),  cex.main=2, col.main="deeppink", cex.axis = 1.2, cex.lab = 1.7,lty=6, frame.plot=FALSE, xaxt="n",yaxt="n",xpd=TRUE, col.lab='royalblue' ))
        # with(datareact_state(), text(year[state==input$state_state & year %in% input$year_state], api[state==input$state_state & year %in% input$year_state], round(api[state==input$state_state & year %in% input$year_state], 2), cex=1.2, col="deeppink", font=2, pos=3,  xpd=TRUE ))
        # axis(1, at=seq(min(datareact_state()$year), max(datareact_state()$year), by=1) , cex.axis=1.0,  lwd=1.7, col="royalblue",  col.axis="royalblue")
        
    })
    
    
    
    # output$t7_g2<-renderPlot({
    #   
    #   tm_shape(datareact7new()) +
    #     tm_borders() +
    #     tm_facets(by="year") +
    #     tm_fill(input$variable7, style="quantile") +
    #     tm_shape(datareact7new()) +
    #     tm_borders(lwd=4) +
    #     tm_facets(by="year")+
    #     tm_layout(panel.label.size = 2.5)
    #   
    # })
    
    
    
    
    
    
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-8___________________________________________#
    #__________________________________________________________________________________#    
    
    
    output$t8_g<-renderPlot({
      req(credentials()$user_auth)  
      
        ggplot(reactiv8(), aes(x = reorder(state,get(input$variable8)))) +
            geom_col(aes(y = get(input$variable8)), fill = "chocolate1", alpha=.7) +coord_flip()+theme_classic()+
            geom_text(aes(y=get(input$variable8), hjust=-0.01,fontface="bold", label=round(get(input$variable8),2)))+labs(x="state", y=input$variable8, title =paste("States Performance For The Year :", input$year8))+
            theme(axis.title = element_text(size=20),plot.title = element_text(size=30, hjust=.5,face="bold"), axis.text = element_text(size=12, family = 8))
        # +
        # annotate("text", size=5.5,x=25, y=max(data_state$input$variable5)+10, colour="blue", label = paste("Year ", input$year5))
        #
        
        
    })
    
    
    
    output$t8_g2<-renderPlot({
      req(credentials()$user_auth)  
      
        tm_shape(data_shape) +
            tm_polygons(paste0(input$variable8,input$year8 ),title=toupper(input$variable8),style=input$style8, palette = input$colstyle8)  +
            tm_borders() +  tm_layout(frame = FALSE)+
            #tm_layout(title = "Quintile Map", title.position = c("right","bottom"))+
            tm_compass(type = "8star", position = c("right", "top"))+tm_layout(legend.outside = TRUE)+
            tm_layout(panel.label.size = 2.5)
        
        
        
        
        
        
    })
    
    
    
    
    
    
    #:::::::::::::::::::::Graph-2
    
    
    output$t8_g3<-renderPlot({
      req(credentials()$user_auth)  
      
        
        ggplot(data_state, aes(x=state, y=get(input$variable8), fill=state)) +geom_boxplot()+theme_classic()+theme(legend.position ="none", plot.title = element_text(size=30, hjust=.5, face="bold"))+coord_flip()+labs(y=input$variable8, x="State",  title =paste("States Performance Over The Years"))+theme(axis.title = element_text(size=20), axis.text = element_text(size=12, family = 8))
    })
    
    
    #:::::::::::::::::::::Graph-4
    
    
    output$t8_g4<-renderPlot({
      req(credentials()$user_auth)  
      
        tm_shape(data_shape) +
            tm_polygons(paste0(input$variable8,input$year8 ),title=toupper(input$variable8),style=input$style8, palette = input$colstyle8)  +
            tm_borders() +  tm_layout(frame = FALSE)+
            #tm_layout(title = "Quintile Map", title.position = c("right","bottom"))+
            tm_layout(legend.outside = TRUE)+
            tm_facets(by="STATENAME")+
            tm_layout(panel.label.size = 2.5)
        
        
        
        
    })
    
    
    
    
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-9___________________________________________#
    #__________________________________________________________________________________#    
    
    
    
    
    
    
    output$t9_g<-renderPlot({
      req(credentials()$user_auth)  
      
        
        ggplot(reactive9(), aes(x=year, y=get(input$variable9)))+ geom_line(aes(col=state), size=1.2)+theme_classic()+xlim(2000,2015)+
            geom_point(col="black", size=1.5)+labs(x="Year", y=input$variable9, title = "Between states Comparison")+theme(plot.title = element_text(size=30, hjust=.5, face="bold"), axis.title = element_text(size=20), axis.text = element_text(size=15))+
            scale_x_continuous(breaks = breaks_width(1))
        
        
        #scale_x_discrete(limits = seq(2000,2091,1))
        
        #+ geom_smooth(col="black", size=1.5, fill="gold1")+theme_classic()+theme(axis.text = element_text(size=18), plot.title=element_text(size=30, face="bold", hjust=.5), axis.title=element_text(size=20))+labs(y=input$variable, x="Year", title="")
        
        #
        # ggplot(reactiv5(), aes(x = reorder(state,get(input$variable5)))) +
        #   geom_col(aes(y = get(input$variable5)), fill = "chocolate1", alpha=.9) +coord_flip()+theme_classic()+
        #   geom_text(aes(y=get(input$variable5), hjust=-0.01, label=round(get(input$variable5),2)))+labs(x="state", y=input$variable5)+
        #   theme(axis.title = element_text(size=20), axis.text = element_text(size=12, family = 8))
        # 
        
        
    })
    
    
    
    
    
    # output$t9_g<-renderPlot({
    # 
    #      ggplot(reactive9(), aes(x = year, y = api, group = state)) +
    #      geom_line(aes(color =    state), size = 1.5) +
    #      geom_point(aes(color =  state), size = 1.5) + theme_classic()+
    #      geom_text(data = reactive9() %>% filter(year ==input$year91), 
    #                aes(label = paste0(state, " - ", round(api,2))) , 
    #                hjust = 1.35, 
    #                fontface = "bold", 
    #                size = 4) +
    #     geom_text(data = reactive9() %>% filter(year==input$year92 ), 
    #             aes(label = paste0(state, " - ", round(api,2))) , 
    #             hjust = -.35, 
    #             fontface = "bold", 
    #             size = 4)+
    #      scale_x_discrete(position = "top") +
    #      theme_bw()+theme(legend.position = "none")+
    #       #Remove the panel border
    #      theme(panel.border     = element_blank()) +
    #       #Remove just about everything from the y axis
    #      theme(axis.title.y     = element_blank()) +
    #      theme(axis.text.y      = element_blank()) +
    #      theme(panel.grid.major.y = element_blank()) +
    #      theme(panel.grid.minor.y = element_blank()) +
    #       #Remove a few things from the x axis and increase font size
    #      theme(axis.title.x     = element_blank()) +
    #      theme(panel.grid.major.x = element_blank()) +
    #      theme(axis.text.x.top      = element_text(size=12)) +
    #       #Remove x & y tick marks
    #      theme(axis.ticks       = element_blank()) +
    #       #Format title & subtitle
    #      theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
    #      theme(plot.subtitle    = element_text(hjust = 0.5)) +
    #        #Labelling as desired
    #      labs(title = "Between states Comparison")
    #        
    # 
    # })
    
    
    
    
    
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-10___________________________________________#
    #__________________________________________________________________________________#    
    
    
    
    output$dataset_state<-renderDataTable({
      req(credentials()$user_auth)
        datareact_data_state()
        
        
        
        
    })
    
    #__________________________________________________________________________________#
    #__________________________________Tab-11___________________________________________#
    #__________________________________________________________________________________#    
    
    reactive11<-reactive({
        data_ratio_state[data_ratio_state$year==input$year11,]
        
        
    })
    
    
    output$t11_g<-renderPlot({
      req(credentials()$user_auth)  
      
        if(input$lab11=="Off") {
            
            ggplot(reactive11(), aes(x = state, y = freq, group = group, fill = group)) +
                geom_bar(stat = "identity", width = 0.75) +
                coord_flip() +facet_grid (~reactive11()$year)+theme_classic()+
                theme(panel.spacing = unit(1, "lines"), strip.text.x=element_text(size=15, colour="black", face="bold"), plot.title = element_text(size=30, hjust=.5, face="bold"), strip.background = element_rect(colour = "black", fill = "seagreen1"), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.text = element_text(size=18),legend.title  = element_text(size=20))+
                scale_fill_manual(values=c("chartreuse", "gray"))+labs(x="Districts", y="Numbers")+
                # geom_text(label=abs(reactive11()$freq), face="bold", col="blue4", size=4)+
                scale_y_continuous(breaks = pretty(reactive11()$freq),labels = abs(pretty(reactive11()$freq)))+labs(title = "Pv-Pf Distribution")
            
        }else{
            
            ggplot(reactive11(), aes(x = state, y = freq, group = group, fill = group)) +
                geom_bar(stat = "identity", width = 0.75) +
                coord_flip() +facet_grid (~reactive11()$year)+theme_classic()+
                theme(panel.spacing = unit(1, "lines"), strip.text.x=element_text(size=15, colour="black", face="bold"), plot.title = element_text(size=30, hjust=.5, face="bold"), strip.background = element_rect(colour = "black", fill = "seagreen1"), axis.title = element_text(size=20), axis.text = element_text(size=12),legend.text = element_text(size=18),legend.title  = element_text(size=20))+
                scale_fill_manual(values=c("chartreuse", "gray"))+labs(x="Districts", y="Numbers")+
                geom_label_repel(label=abs(reactive11()$freq), fontface = "bold", size=3)+
                scale_y_continuous(breaks = pretty(reactive11()$freq),labels = abs(pretty(reactive11()$freq)))+labs(title = "Pv-Pf Distribution")
            
            
            
        }
        
    })
    
    
    
    
    
    
    
    output$download_data_state<-downloadHandler(
        
      
        filename = function(){
            
            paste("dataset_state", "csv", sep=".")
            
        },
        
        content=function(file){
            
            
            write.csv(datareact_data_state(),file)
            
        }
        
    )
    
    
    
    
    
    #__________________________________________________________________________________#
    #__________________________________Tab-13___________________________________________#
    #__________________________________________________________________________________#    
    
    reactive13<-reactive({
      req(credentials()$user_auth)  
      
        data_india[data_india$year_ %in% input$year13 & data_india$STATENAME %in% input$state13,]
        
        
    })
    
    output$t13_g<-renderPlot({
        tm_shape(reactive13()) +
            tm_polygons(input$variable13,title=toupper(input$variable13),style=input$style13, palette = input$colstyle13)  +
            tm_borders() +  tm_layout(frame = FALSE)+
            #tm_layout(title = "Quintile Map", title.position = c("right","bottom"))+
            tm_layout(legend.outside = TRUE)+
            tm_facets(by="year_")+
            tm_layout(panel.label.size = 2.0)
        
        
        # breaks<-seq(0, 12, by=2)
        # output$t13_g<-renderPlot({
        #   tm_shape(reactive13()) +
        #     tm_polygons(input$variable13,title=toupper(input$variable13), breaks = breaks)  +
        #     tm_borders() +  tm_layout(frame = FALSE)+
        #     #tm_layout(title = "Quintile Map", title.position = c("right","bottom"))+
        #     tm_layout(legend.outside = TRUE)+
        #     tm_facets(by="year_")+
        #     tm_layout(panel.label.size = 2.0) 
        
    })
    
    
    
    
    
    # output$t11_g<-renderPlot({
    #   
    #   
    #   ggplot(reactive11(), aes(x=year, y=get(input$variable11)))+ geom_point(col="deeppink", size=3) + geom_smooth(col="black", size=1, fill="gold1")+theme_classic()+theme(axis.text = element_text(size=18), plot.title=element_text(size=30, face="bold", hjust=.5), axis.title=element_text(size=20))+labs(y=input$variable11, x="Year", title=paste("Country", input$variable11, "Over The Years"))
    #   
    #   #
    #   # ggplot(reactiv5(), aes(x = reorder(state,get(input$variable5)))) +
    #   #   geom_col(aes(y = get(input$variable5)), fill = "chocolate1", alpha=.7) +coord_flip()+theme_classic()+
    #   #   geom_text(aes(y=get(input$variable5), hjust=-0.01, label=round(get(input$variable5),2)))+labs(x="state", y=input$variable5)+
    #   #   theme(axis.title = element_text(size=20), axis.text = element_text(size=12, family = 8))
    #   # 
    #   
    #   
    # })
    
    
    
    # output$t11_g2<-renderPlot({
    #   
    #   ggplot(data, aes(year, y=get(input$variable11))) +geom_jitter()+theme_classic()+scale_x_continuous(c(2000, 20111),breaks = breaks_width(1))
    #     
    # 
    # 
    #   
    # })
    
    
    # output$t11_g3<-renderPlot({
    #   
    #   
    #   data %>%
    #     mutate(district = fct_reorder(district, desc(get(input$variable11)))) %>%
    #     ggplot(aes(district, y=get(input$variable11))) +geom_bar(stat="identity")+theme_classic()+theme(axis.text.x = element_text(angle = 110, vjust = 0.5, hjust=1))
    #     
    #   
    #   
    #   
    # })
    
    
    
    # output$download_api<-downloadHandler(
    #   
    #   filename = function(){
    #     
    #     paste("api", "png", sep=".")
    #     
    #   },
    #   
    #   content=function(file){
    #     
    #     png(file)
    #     
    #     with(data, plot(year[state==input$state & district==input$district & year %in% input$year], api[state==input$state & district==input$district  & year %in% input$year], type="o", lwd=3, col="violetred",  ylab="API", xlab="YEAR",main= "Annual Parasite Index (API) Over the Years",  cex.main=2, col.main="violetred", cex.axis = 1.2, cex.lab = 1.5,lty=6, frame.plot=FALSE, xaxt="n",yaxt="n",xpd=TRUE )) 
    #     with(data, text(year[state==input$state & district==input$district & year %in% input$year], api[state==input$state & district==input$district & year %in% input$year], api[state==input$state & district==input$district  & year %in% input$year], cex=1.2, col="violetred", font=2, pos=3,  xpd=TRUE ))
    #     axis(1, at=seq(2000, 2017, by=1) , labels= seq(2000, 2017, by=1),cex.axis=1.0,  lwd=1.5)
    #     with(data, axis(2, at=api[state==input$state & district==input$district  & year %in% input$year] ,cex.axis=1.0,  lwd=1.5))
    #     
    #     dev.off()
    #     
    #   }
    # 
    # )
    
}



# data<-with(data, data[order(-api),])  
# data[which(data$year==20011 & data$state=="BIHAR"), c( "state", "district", "year",  "api")]




# Run the application 
shinyApp(ui = ui, server = server)
