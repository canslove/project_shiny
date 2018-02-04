#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


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
sidebar <- dashboardSidebar( background = "black",
  sidebarUserPanel(em('Donghyun Kang'), subtitle = tags$i("Data Scientist"),
                   image="https://avatars1.githubusercontent.com/u/35157973?s=460&v=4"),
  
  sidebarMenu(
    #menuItem("MOtivation", tabName = "motive", icon = icon("headphones")), # introduction
    menuItem("Sourses", tabName = "sourses", icon = icon("headphones")), # Summary of data sets
    menuItem("Insights", tabName = "insights", icon = icon("list")), # Points
    menuItem("DataTable", tabName = "datatable", icon = icon("table")), # On analysis
    menuItem("Analysis", tabName = "analysis", icon = icon("pencil")), # On analysis
    menuItem("Discovery", tabName = "discovery", icon = icon("binoculars")), #To ....
    menuItem("Limitation", tabName = "limitation", icon = icon("exclamation")), #In limit...
    menuItem("Future Work", tabName = "futurework", icon = icon("flask")), #Future Work
    menuItem("Nationwide", tabName = "map", icon = icon("map-o")), # Y.....Ymap ???
    #menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Worldwide", tabName = "worldmap", icon = icon("map-marker")),
    #menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    #menuItem("Source code", icon = icon("file-code-o"), 
    #         href = "https://github.com/canslove/project_shiny"),

    #box(title = "Controls1", background = "black", solidHeader = TRUE, width = 12,
    box(background = "black", width = 12, collapsible = TRUE,
        selectizeInput(inputId = "selected", label = "Item in Dataset(1) :", choice)
    ) #box
    
    
    
  ) # sidebarMenu
  
  
  
  
)

## Body content ------------------------
body <- dashboardBody(
  # tags$head(tags$style(HTML('
  #       /* logo */
  #                           .skin-blue .main-header .logo {
  #                           background-color: #f4b943;
  #                           }
  #                           
  #                           /* logo when hovered */
  #                           .skin-blue .main-header .logo:hover {
  #                           background-color: #f4b943;
  #                           }
  #                           
  #                           /* navbar (rest of the header) */
  #                           .skin-blue .main-header .navbar {
  #                           background-color: #f4b943;
  #                           }        
  #                           
  #                           /* main sidebar */
  #                           .skin-blue .main-sidebar {
  #                           background-color: #f4b943;
  #                           }
  #                           
  #                           /* active selected tab in the sidebarmenu */
  #                           .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
  #                           background-color: #ff0000;
  #                           }
  #                           
  #                           /* other links in the sidebarmenu */
  #                           .skin-blue .main-sidebar .sidebar .sidebar-menu a{
  #                           background-color: #00ff00;
  #                           color: #000000;
  #                           }
  #                           
  #                           /* other links in the sidebarmenu when hovered */
  #                           .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
  #                           background-color: #ff69b4;
  #                           }
  #                           /* toggle button when hovered  */                    
  #                           .skin-blue .main-header .navbar .sidebar-toggle:hover{
  #                           background-color: #ff69b4;
  #                           }
  #                           '))),
  # 
  tabItems(
    # tabItem(tabName = "motive",
    #         fluidPage(
    #           tagList(
    #             globeOutput("globe"),    
    #             div(id="info", tagList(
    #               includeHTML('help.html'),
    #               radioButtons("select", 
    #                            label = h3(" "), 
    #                            choices = list("2013-14 International Students" = 1,                                                  
    #                                           "2012-13 US Students Studying Abroad" = 2), selected = 1)
    #             )
    #             )#div
    #           ) #tagList
    #         )# fluidPage
    # ), # tabItem
    # Summary of data sets
    tabItem(tabName = "sourses",
            fluidPage(theme = shinytheme(ShinyThemeName),
              #h3("Summary of the data sets"),
              # box(
              #   title = "Summary of the data sets", background = Titlecolor, solidHeader = TRUE
              # ),#box
              h1("Intelligent Music Streaming Service"),
              em(" based on the analysis of"),
              h3(" Top streams-Songs and Artists on Spotify "),
              img(src="http://www.bassheadspeakers.com/wp-content/uploads/2016/07/Best-over-ear-bluetooth-headphones-of-2016-820x461.jpg"),
              br(),
              h4("Donghyun Kang (dhyun.kang@gmail.com)")
            )#fluidPage
    ),#tabItem-Sources
    
    # Insights - tab? box? text?
    tabItem(tabName = "insights",
            fluidRow(theme = shinytheme(ShinyThemeName),
                     column(width = 6,
                            box(
                              title = "Insights", width = NULL, background = Titlecolor, solidHeader = TRUE, status = "primary",
                              tags$i(h4("Spotify Investor Says Streaming Will Quadruple by 2025. Any Questions?")),
                              hr(),
                              h3("Predict"),
                              h5("- what type of songs will be popular in the future?"),
                              h3("Share"),
                              h5("- how are popular songs propagate country by country?"),
                              h3("Business Models"),
                              h5("- is there any tendency or trend in this field?"),
                              h3("Influence Map"),
                              h5("- how long does a top ranked songs take to get into the ranking of neighbor countries?"),
                              h3("Recommendation Servie"),
                              h5("- is this possible? Any pattern?")
                            ) #box
                     ), #column
                     column(width = 6,
                            img(src="https://static1.squarespace.com/static/54c02777e4b022a64cd11524/t/553569a7e4b07ea1b733fd16/1429563818269/", width = 630, height = 460)
                     )#column
            )#fluidRow
    ),##tabItem-Insights
    
    tabItem(tabName = "datatable",
            fluidRow(theme = shinytheme(ShinyThemeName),
                     box(title = "Worldwide Top 100 Ranked Songs (Jan/01/2017 ~ Jan/01/2018)",
                         #background = Titlecolor, 
                         solidHeader = TRUE, #status = "primary",
                         DT::dataTableOutput("table1"), width = 12)
            )#fluidRow
    ),#tabItem-Sources
    
    # Analysis
    tabItem(tabName = "analysis",
            # tab box
            fluidRow(theme = shinytheme(ShinyThemeName),
              box(
                #title = "Controls",# background = Titlecolor,solidHeader = TRUE, status = "primary", 
                width = 3,
                selectInput(inputId = "input_type", label = "Switch to checkbox-> Select Regions:",
                            choices = c("slider", "checkboxGroup", "radioButtons", "date", "daterange")), # selectInput
                uiOutput("ui"), # This outputs the dynamic UI component wrt selectInput
                # Summary of selectInput
                box(#background = "navy", 
                  width = 12,
                    tags$p("Input type:"),
                    verbatimTextOutput("input_type_text"),
                    tags$p("Dynamic input value:"),
                    verbatimTextOutput("dynamic_value")
                ), # box
                
                #sliderInput(inputId = "slider0", "Ranking (range) to see:", min = 1, max = 100, value = c(1, 50)),
                
                dateRangeInput(inputId = "Id_date", ("Date range (For DT, 1st date selection only)"), #cf. storng("") -> bold
                               start = "2017-01-01", end = "2017-01-31",
                               min = "2017-01-01", max = "2018-01-31"),
                # "RANKING"    "TRACK_NAME" "ARTIST"     "STREAMS"    "DATE"       "REGION"
                # radioButtons(inputId = "Id_man_or_song", label = h5("Radio buttons"),
                #              choices = list("ARTIST" = "ARTIST", "Track Name " = "TRACK_NAME"), 
                #              selected = "ARTIST"),
                checkboxInput(inputId = "Id_man_or_song", label = ("V for ARTIST(or TrackName)"), value = TRUE),
                
                selectInput(inputId = "x", label = ("ARTIST"), choices = unique(raw.df$ARTIST),
                            selected = "Maroon 5"),
                selectInput(inputId = "y", label = ("TRACK_NAME"), choices = unique(raw.df$TRACK_NAME),
                            selected = "Don't Wanna Know"),
                # selectInput(inputId = "Id_plotType", label = h5("Select Plot type"), 
                #             choices = list("Scatter" = 1, "Bar" = 2, "Histogram" = 3, "Distribution" = 3), 
                #             selected = 1),
                selectInput(inputId = "Id_varY", label = ("Select variable to See"), 
                            choices = c("Ranking" = 'RANKING', "Number of streams" = 'STREAMS'), 
                            selected = "RANKING"),
                
                
                # Select whether to overlay smooth trend line
                checkboxInput(inputId = "smoother", label = ("Overlay smooth trend line"), value = FALSE),
                # Display only if the smoother is checked
                conditionalPanel(condition = "input.smoother == true",
                                 sliderInput(inputId = "f", label = "Smoother span:",
                                             min = 0.01, max = 1, value = 0.67, step = 0.01,
                                             animate = animationOptions(interval = 100)),
                                 HTML("Higher values give more smoothness.") ) # conditionalPanel
                # checkboxGroupInput(inputId = "Id_RegionSel", label = ("Select Regions"), 
                #                    choices = list_RegionMap, selected = c("us","gb","fr","ec")),
                
                
              ), # box
              
              
              tabBox(
                #title = "Top Rank Songs",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "650px", width = 9, # width total =12 = 3 + 9
                # tabPanel("(1) ReducedTable", icon = icon("table"), 
                #          #box(DT::dataTableOutput("table1"), width = 12),
                #          box(DT::dataTableOutput("table2"), width = 12)
                # ), # tabPanel
                
                tabPanel("(1) Var-Chart", icon = icon("line-chart"), # map rank region -> dybamic (side bar)
                         box(#title = "Chart", 
                           #background = "black", 
                           solidHeader = TRUE, collapsible = TRUE, width = 4,
                           selectInput(inputId = "Id_plotType", label = ("Select Plot type"), 
                                       choices = c("Scatter", "Bar", "Histogram", "Distribution"), 
                                       selected = "Bar")
                         ),
                         box(#background = "black", 
                           width = 2, collapsible = TRUE, collapsed = FALSE,
                             selectInput('facet_row', 'Facet Row', c(None = '.', nmRawdf), selected = "")
                         ),#box
                         box(#background = "black", 
                           width = 2, collapsible = TRUE, collapsed = FALSE,
                             selectInput('facet_col', 'Facet Col', c(Noen = '.', nmRawdf), selected = "REGION")
                         ),#box
                         box(#background = "navy", 
                           width = 4,
                             tags$p("Selected Region:"),
                             verbatimTextOutput("sel_region")
                         ), # box

                         box( #title = "Chart", 
                           #background = "black", 
                           solidHeader = TRUE, collapsible = TRUE, width = 12,
                           textOutput(outputId = "descplotRank"), # description
                           plotOutput(outputId = "plotRank", height = 350)  #350
                           
                         ), # box
                         
                         box(#background = "black", 
                           width = 2, collapsible = TRUE, collapsed = FALSE,
                             checkboxGroupInput(inputId = "Id_RegionSel", label = ("Select Regions"),
                                                choices = list_RegionMap, selected = c("us","gb","fr","ec"))
                         )#box
                         
                ), #tabPanel
                
                tabPanel("(2) Graph", icon = icon("chart-area"), # map rank region -> dybamic (side bar)
                         box( #title = "Chart", 
                           background = "black", solidHeader = TRUE, collapsible = TRUE, width = 12,
                           checkboxInput("summaryplot100", "Show Summary", TRUE),
                           textOutput(outputId = "descplot100"), # description
                           plotOutput(outputId = "plot100", height = 350)
                           #verbatimTextOutput("summaryplot100")
                         ), # box
                         
                         box(#title = "Chart", 
                           background = "black", solidHeader = TRUE, collapsible = TRUE, width = 12,
                           textOutput(outputId = "descplot1000"), # description
                           plotOutput(outputId = "plot1000", height = 350)
                         ), # box
                         
                         # box(
                         #   plotOutput("plot1", height = 350)
                         # ), # box
                         
                         box(#title = "Chart", 
                           background = "black", solidHeader = TRUE, collapsible = TRUE, width = 4,
                           sliderInput('plotHeight', 'Height of plot (in pixels)', 
                                       min = 100, max = 2000, value = 1000)
                         )
                         
                ), #tabPanel
                
                tabPanel("(3) Map", icon = icon("map-marker"), # map rank region -> dybamic (side bar)
                         box(
                           title = "Chart", 
                           #background = "fuchsia", 
                           solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           radioButtons(inputId = "Id_mapshow", label = ("Select to see :"),
                                        choices = c("Average Ranking in every Region" = "showRanking", 
                                                       "The day of highest ranked " = "showDay"),
                                        selected = "showRanking")
                           
                         ), # box
                         box(
                           title = "TOP Ranks Music Distibuted Regions", 
                           #background = "maroon", 
                           solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           
                           leafletOutput("mymap"),
                           p(),
                           actionButton("recalc", "New points")
                         )# box
                ), #tabPanel
                
                tabPanel("(4) Word Cloud", icon = icon("cloud-download"),
                         box(
                           title = "Word Cloud", 
                           #background = "yellow", 
                           solidHeader = TRUE,
                           collapsible = TRUE, width = 8,
                           plotOutput("plot200")
                         ), # box
                         box(
                           title = "Word cloud control", 
                           #background = "blue", 
                           solidHeader = TRUE,
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
            fluidRow(theme = shinytheme(ShinyThemeName),
              tabBox(
                title = "DataSets",
                side = "right",
                # The id lets us use input$tabset2 on the server to find the current tab
                id = "tabset100", height = "250px",
                tabPanel("Data", icon = icon("table"),
                         "spotifys-worldwide-daily-song-rankig.zip", tags$br(),
                         "top-tracks-of-2017.zip", tags$br(),
                         "top-tracks-of-2016.zip", tags$br(),
                         "every-song-you-have-heard-almost.zip"),
                tabPanel("Summary", verbatimTextOutput("summary"))
              ),
              tabBox(
                title = "Categorized by",
                side = "right", height = "250px",
                id = "tabset200", 
                selected = "Artist",
                tabPanel("Artist"),#, plotOutput("plot_art")),
                tabPanel("Genre"),#, plotOutput("plot_art")),
                tabPanel("Gender"),#, plotOutput("plot_art")),
                tabPanel("Age"),#, plotOutput("plot_art")),
                tabPanel("Season"),#, plotOutput("plot_art")),
                tabPanel("Lyrics", "Note that these results came from limited data")
              )
            ),#fluidRow
            fluidRow(
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("user-circle-o"), "Tendency"),
                tabPanel("Tab1",
                         "There are several points we can point out ...:",
                         verbatimTextOutput("tabset1Selected"),
                         img(src="https://www.virtuslaw.com/wp-content/uploads/2017/07/What-is-a-Limitation-of-Remedy-Clause.jpg")
                ),
                tabPanel("Tab2", "Tab content 2")
              )
            )#fluidRow
    ),#tabItem-Discovery
    
    # Limitation - Blind Spots
    tabItem(tabName = "limitation",
            # Boxes need to be put in a row (or column)
            fluidRow(theme = shinytheme(ShinyThemeName),
                     column(width = 6,
                            box( title = "Blind spots", width = NULL, background = Titlecolor, solidHeader = TRUE, status = "primary",
                                 h3("Data Set is Too huge to Visualize"),
                                 em("To find more reliable statistic"),
                                 br(),
                                 em("all dataset should not be reduced !!")
                                 
                                ) #box
                            ), #column
                     column(width = 6,
                            img(src="http://images.clipartpanda.com/limitation-clipart-limitation-plane1.jpg", width = 630, height = 460)
                            )#column
            )#fluidRow
    ),#tabItem-Limitation
    
    # Future Work
    tabItem(tabName = "futurework",
            fluidRow(theme = shinytheme(ShinyThemeName),
                     column(width = 6,
                            box(
                              title = "Further Work", width = NULL, background = Titlecolor, solidHeader = TRUE, status = "primary",
                              h3("Analysis"),
                              h5("- Nationwide distribution"),
                              h5("- Worldwide Delayspread"),
                              br(),
                              br(),
                              h3("Reference Comparison"),
                              h5("- Web Scraping from other streaming service providers"),
                              h5("- Data mining for more data sets relate to this project"),
                              tags$i("ex) daily weather info., social big issue"),
                              br(),
                              br(),
                              h3("Predict Model Development")
                            ) #box
                     ), #column
                     column(width = 6,
                            img(src="http://unidosxisrael.org/wp-content/uploads/2016/12/19los10consejos.jpg")
                     )#column
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
            
            bootstrapPage(theme = shinytheme(ShinyThemeName),
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
            fluidRow(theme = shinytheme(ShinyThemeName)
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


ui <- dashboardPage(header, sidebar, body, skin = "green") # blue, black, purple, green, red, yellow, 
