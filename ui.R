
## Header content ----------------------
header <- dashboardHeader(
  #title = "Top Streamed Songs on Spotify", titleWidth = 450, #icon = icon("headphones"),
  title = "Analysis of Top Streamed Songs and Artists on Spotify", titleWidth = 600, 
  # Messages menu
  # dropdownMenu(type = "messages",
  #              messageItem(
  #                from = "Sales Dept",
  #                message = "Sales are steady this month."
  #              ),
  #              messageItem(
  #                from = "New User",
  #                message = "How do I register?",
  #                icon = icon("question"),
  #                time = "13:45"
  #              ),
  #              messageItem(
  #                from = "Support",
  #                message = "The new server is ready.",
  #                icon = icon("life-ring"),
  #                time = "2014-12-01"
  #              )
  # ),
  #dropdownMenuOutput("messageMenu"),
  # Notification
  # dropdownMenu(type = "notifications",
  #              notificationItem(
  #                text = "5 new users today",
  #                icon("users")
  #              ),
  #              notificationItem(
  #                text = "12 items delivered",
  #                icon("truck"),
  #                status = "success"
  #              ),
  #              notificationItem(
  #                text = "Server load at 86%",
  #                icon = icon("exclamation-triangle"),
  #                status = "warning"
  #              )
  # ),#dropdownMenu
  # Task

  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 70, color = "green", "Analysis"),
               taskItem(value = 50, color = "aqua",  "Discovery"),
               taskItem(value = 40, color = "yellow","Limitation"),
               taskItem(value =  0, color = "red", "FutureWork" )
  )#dropdownMenu
)#dashboardHeader




## icon refer to : http://fontawesome.io/icons/
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sourses", tabName = "sourses", icon = icon("headphones")), # Summary of data sets
    menuItem("Insights", tabName = "insights", icon = icon("list")), # Points
    menuItem("Analysis", tabName = "analysis", icon = icon("pencil")), # On analysis
    menuItem("Discovery", tabName = "discovery", icon = icon("binoculars")), #To ....
    menuItem("Limitation", tabName = "limitation", icon = icon("exclamation")), #In limit...
    menuItem("Future Work", tabName = "futurework", icon = icon("flask")), #Future Work
    menuItem("YMap", tabName = "map", icon = icon("map-o")), # Y.....Ymap ???
    #menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Worldmap", tabName = "worldmap", icon = icon("map-marker")),
    #menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/canslove/project_shiny"),
             #href = "https://github.com/rstudio/shinydashboard/")
    
    #box(title = "Controls1", background = "black", solidHeader = TRUE, width = 12,
    box(background = "black", width = 12, collapsible = TRUE,
        selectizeInput(inputId = "selected", label = "Item in Dataset(1) :", choice)
    ) #box
    
    
    
  ) # sidebarMenu


  
  
)

## Body content ------------------------
body <- dashboardBody(
  tabItems(
    # Summary of data sets
    tabItem(tabName = "sourses",
            fluidPage(
              #h3("Summary of the data sets"),
              # box(
              #   title = "Summary of the data sets", background = Titlecolor, solidHeader = TRUE
              # ),#box
              img(src="http://www.bassheadspeakers.com/wp-content/uploads/2016/07/Best-over-ear-bluetooth-headphones-of-2016-820x461.jpg")
            )#fluidPage
    ),#tabItem-Sources

    # Insights - tab? box? text?
    tabItem(tabName = "insights",
            # tab box
            fluidRow(
              box(
                title = "Insights", background = "black", solidHeader = TRUE,
                width = 3,
                selectizeInput(inputId = "origin",
                               label = "Departure airport",
                               choices = unique(flights[, origin])),
                selectizeInput(inputId = "dest",
                               label = "Arrival airport",
                               choices = unique(flights[, dest]))
              ), # box
              
              tabBox(
                id = "tabid.in.0", height = "650px", width = 9, # width total =12 = 3 + 9
                tabPanel(icon = icon("bar-chart"), "Visulaize the Data",
                         box(
                           title = "Histogram", background = "olive", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           #plotOutput("plot1", height = 350)
                           column(6, plotOutput("count")),
                           column(6, plotOutput("delay"))
                         ) # box
                ) #tabPanel
              ) # box
            ) # fluidRow
    ),##tabItem-Insights
    
    # Analysis
    tabItem(tabName = "analysis",
            # tab box
            fluidRow(
              box(
                #title = "Controls2", 
                background = "black", solidHeader = TRUE, width = 3,
                sliderInput(inputId = "slider0", "Ranking (range) to see:", min = 1, max = 100, value = c(1, 50)),
                
                dateRangeInput(inputId = "Id_date", strong("Date range (For DT, 1st date selection only)"), 
                               start = "2017-01-01", end = "2017-01-31",
                               min = "2017-01-01", max = "2018-01-31"),
                # "RANKING"    "TRACK_NAME" "ARTIST"     "STREAMS"    "DATE"       "REGION"
                # radioButtons(inputId = "Id_man_or_song", label = h5("Radio buttons"),
                #              choices = list("ARTIST" = "ARTIST", "Track Name " = "TRACK_NAME"), 
                #              selected = "ARTIST"),
                checkboxInput(inputId = "Id_man_or_song", label = strong("V for ARTIST(or TrackName)"), value = TRUE),
                
                selectInput(inputId = "x", label = strong("ARTIST"), choices = unique(raw.df$ARTIST),
                            selected = "Maroon 5"),
                selectInput(inputId = "y", label = strong("TRACK_NAME"), choices = unique(raw.df$TRACK_NAME),
                            selected = "Don't Wanna Know"),
                # selectInput(inputId = "Id_plotType", label = h5("Select Plot type"), 
                #             choices = list("Scatter" = 1, "Bar" = 2, "Histogram" = 3, "Distribution" = 3), 
                #             selected = 1),
                selectInput(inputId = "Id_varY", label = h5("Select variable to See"), 
                            choices = list("Ranking" = "RANKING", "Number of streams" = "STREAMS"), 
                            selected = "RANKING"),
                selectInput(inputId = "input_type", label = "Switch to checkbox-> Select Regions:",
                            choices = c("slider", "checkboxGroup", "radioButtons", "date", "daterange")), # selectInput
                uiOutput("ui"), # This outputs the dynamic UI component wrt selectInput
                
                                # Select whether to overlay smooth trend line
                checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                # Display only if the smoother is checked
                conditionalPanel(condition = "input.smoother == true",
                                 sliderInput(inputId = "f", label = "Smoother span:",
                                             min = 0.01, max = 1, value = 0.67, step = 0.01,
                                             animate = animationOptions(interval = 100)),
                                 HTML("Higher values give more smoothness.") ), # conditionalPanel
                checkboxGroupInput(inputId = "Id_RegionSel", label = h5("Select Regions"), 
                                    choices = list_RegionMap, selected = c("us","gb","fr","ec")),

                # Summary of selectInput
                box(background = "orange", width = 12,
                  tags$p("Input type:"),
                  verbatimTextOutput("input_type_text"),
                  tags$p("Dynamic input value:"),
                  verbatimTextOutput("dynamic_value")
                ) # box
              ), # box
              

              tabBox(
                #title = "Top Rank Songs",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "650px", width = 9, # width total =12 = 3 + 9
                tabPanel("(1) Table", icon = icon("table"), 
                         box(DT::dataTableOutput("table1"), width = 12),
                         box(DT::dataTableOutput("table2"), width = 12)
                ), # tabPanel
                
                
                
                tabPanel("(2) Chart", icon = icon("line-chart"), # map rank region -> dybamic (side bar)
                         box(#title = "Chart", 
                             background = "blue", solidHeader = TRUE, collapsible = TRUE, width = 4,
                             selectInput(inputId = "Id_plotType", label = h5("Select Plot type"), 
                                         choices = list("Scatter" = 1, "Bar" = 2, "Histogram" = 3, "Distribution" = 3), 
                                         selected = 1)
                         ),
                         box(#title = "Chart", 
                             background = "blue", solidHeader = TRUE, collapsible = TRUE, width = 4,
                             sliderInput('plotHeight', 'Height of plot (in pixels)', 
                                         min = 100, max = 2000, value = 1000)
                         ),
                         box(background = "blue", width = 2, collapsible = TRUE, collapsed = FALSE,
                             selectInput('facet_row', 'Facet Row', c(None = '.', nmRawdf), selected = "")
                         ),#box
                         box(background = "blue", width = 2, collapsible = TRUE, collapsed = FALSE,
                             selectInput('facet_col', 'Facet Col', c(Noen = '.', nmRawdf), selected = "REGION")
                         ),#box
                         
                         box( #title = "Chart", 
                           background = "blue", solidHeader = TRUE, collapsible = TRUE, width = 12,
                           
                           plotOutput(outputId = "plotRank", height = 350),
                           textOutput(outputId = "desc"), # description
                           plotOutput("plot1", height = 350),
                           plotOutput("plot100", height = 350)
                         ) # box
                      
                ), #tabPanel

                tabPanel("(4) Map", icon = icon("map-marker"), # map rank region -> dybamic (side bar)
                         box(
                           title = "Chart", background = "fuchsia", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           radioButtons(inputId = "Id_mapshow", label = h5("Select to see :"),
                                        choices = list("Average Ranking in every Region" = "showRanking", 
                                                       "The day of highest ranked " = "showDay"),
                                        selected = "showRanking")

                         ), # box
                         box(
                           title = "TOP Ranks Music Distibuted Regions", background = "maroon", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           
                           leafletOutput("mymap"),
                           p(),
                           actionButton("recalc", "New points")
                         )# box
                ), #tabPanel
                
                tabPanel("(5) Word Cloud", icon = icon("cloud-download"),
                         box(
                           title = "Word Cloud", background = "yellow", solidHeader = TRUE,
                           collapsible = TRUE, width = 8,
                           plotOutput("plot200")
                         ), # box
                         box(
                           title = "Word cloud control", background = "blue", solidHeader = TRUE,
                           collapsible = TRUE, width = 4,
                           selectInput("selection", "Choose a book:",
                                       choices = books),
                           actionButton("update", "Change"),
                           hr(),
                           sliderInput("freq",
                                       "Minimum Frequency:",
                                       min = 1,  max = 50, value = 15),
                           sliderInput("max",
                                       "Maximum Number of Words:",
                                       min = 1,  max = 300,  value = 100)
                         )#box
                ) #tabPanel
                
              ) # tabbox
            ) # fluidRow
      ),#tabItem-Analysis
   
    # Discovery
    tabItem(tabName = "discovery",
            # tab box
            fluidRow(
              tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset2 on the server to find the current tab
                id = "tabset2", height = "250px",
                tabPanel("Tab1", "First tab content", icon = icon("table")),
                tabPanel("Tab2", "Tab content 2")
              ),
              tabBox(
                side = "right", height = "250px",
                selected = "Tab3",
                tabPanel("Tab1", "Tab content 1"),
                tabPanel("Tab2", "Tab content 2"),
                tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
              )
            ),#fluidRow
            fluidRow(
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel("Tab1",
                         "Currently selected tab from first box:",
                         verbatimTextOutput("tabset1Selected")
                ),
                tabPanel("Tab2", "Tab content 2")
              )
            )#fluidRow
    ),#tabItem-Discovery
    
    # Limitation - Blind Spots
    tabItem(tabName = "limitation",
            # Boxes need to be put in a row (or column)
            fluidRow(
              box(
                title = "Blind spots", background = Titlecolor, solidHeader = TRUE
              )#box
            )#fluidRow
    ),#tabItem-Limitation
    
    # Future Work
    tabItem(tabName = "futurework",
            # Boxes need to be put in a row (or column)
            fluidRow(
              box(
                title = "Further Work", background = Titlecolor, solidHeader = TRUE
              )#box
            )#fluidRow
    ),#tabItem-Futurework
    
    # Map
    tabItem(tabName = "map",
            selectizeInput("selected2", "Select Item in Dataset(1) :", choice2),
            # map/hist selectized input start --------------------
            # using infoBox
            fluidRow(infoBoxOutput("maxBox"),
                     infoBoxOutput("minBox"),
                     infoBoxOutput("avgBox")),
            # gvisGeoChart
            fluidRow(box(htmlOutput("map"), height = 450),
                     # gvisHistoGram
                     box(htmlOutput("hist"), height = 450))
            # map/hist selectized input end --------------------
    ),#tabItem-Map

    # Worldmap
    tabItem(tabName = "worldmap",
            # map-start ---------------------
            # fluidPage(
            #   leafletOutput("worldmap"),
            #   br(),
            #   checkboxInput("show", "Show States", value = FALSE)
            # ), #fluidPage
            
            bootstrapPage(
              tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
              leafletOutput("worldmap"),
              #leafletOutput("worldmap", width = "100%", height = "100%"),
              absolutePanel(top = 8, right = 8,
                            sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                                        value = range(quakes$mag), step = 0.1
                            ),
                            selectInput("colors", "Color Scheme",
                                        rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                            ),
                            checkboxInput("legend", "Show legend", TRUE)
              )#absolutePanel
            )#bootstrapPage
            
            # map-end ---------------------
    ),#tabItem-Worldmap
    
    # Widgets
    tabItem(tabName = "widgets",
            h2("Widgets tab content"),
            fluidRow(
              #leafletOutput("dymap"),
              # absolutePanel(top = 10, right = 10,
              #               sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
              #                           value = range(quakes$mag), step = 0.1
              #               ),
              #               selectInput("colors", "Color Scheme",
              #                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
              #               ),
              #               checkboxInput("legend", "Show legend", TRUE)
              #   )#absolutePanel
              
              # Some conflicts happens when I use this bootstrapPage !!! -> Later
              # == http://rstudio.github.io/leaflet/shiny.html ==
              # bootstrapPage(
              #   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
              #   leafletOutput("dymap", width = "100%", height = "100%")
              #   absolutePanel(top = 10, right = 10,
              #                 sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
              #                             value = range(quakes$mag), step = 0.1
              #                 ),
              #                 selectInput("colors", "Color Scheme",
              #                             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
              #                 ),
              #                 checkboxInput("legend", "Show legend", TRUE)
              #   )#absolutePanel
              # )#bootstrapPage
              
              # column(3, 
              #        wellPanel(
              #          selectInput("input_type", "Input type",
              #               c("slider", "text", "numeric", "checkbox",
              #                 "checkboxGroup", "radioButtons", "selectInput",
              #                 "selectInput (multi)", "date", "daterange"
              #               )
              #          )
              #        ),
              #        wellPanel(
              #          # This outputs the dynamic UI component
              #          uiOutput("ui")
              #        ),
              #        
              #        tags$p("Input type:"),
              #        verbatimTextOutput("input_type_text"),
              #        tags$p("Dynamic input value:"),
              #        verbatimTextOutput("dynamic_value")
              # ) # column
            ) #fluidRow
    ) # tabItem
  ) #tabItem-Widgets
  

  
) # dashboardBody


ui <- dashboardPage(header, sidebar, body)