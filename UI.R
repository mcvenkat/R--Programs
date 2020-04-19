shinyUI(dashboardPage(
  
  dashboardHeader(title = "The Pleasing Ratio Project", 
                  titleWidth = 280),
  
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Play!",       tabName = "play", icon = icon("play")),
                menuItem("Results",     tabName = "resu", icon = icon("bar-chart")),          
                menuItem("The Project", tabName = "proj", icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }", functions = c("refresh")),
    tabItems(
      tabItem(tabName = "proj", 
              fluidRow(column(width = 12, 
                              style='padding:10px;', 
                              includeMarkdown("files/project.md")))),
      
      tabItem(tabName = "play", 
              fluidRow(column(width = 12, 
                              style='padding:10px;', 
                              includeMarkdown("files/briefing.md"))),
              fluidRow( 
                box(title = 'Option A', 
                    status = "primary", 
                    solidHeader = TRUE,
                    plotOutput("A_plot", height = "180px")),
                box(title = 'Option B', 
                    status = "primary", 
                    solidHeader = TRUE,
                    plotOutput("B_plot", height = "180px"))
              ),
              fluidRow(
                column(12, align="center", 
                       radioButtons("choice", 
                                    "Which one do you like most?",
                                    c("Option A" = "A",
                                      "Option B" = "B"), 
                                    inline = TRUE, 
                                    selected=character(0)))
              ),
              
              fluidRow(
                column(12, 
                       align="center", 
                       conditionalPanel(
                         condition = "input.choice == 'A'||
                         input.choice == 'B'||
                         input.choice == 'N'",
                         uiOutput("vote")))),
              fluidRow(
                column(12, align="center", div(style = "height:10px;"), 
                       conditionalPanel(
                         condition = "input.vote == '1'",
                         HTML("<b>Thank you!</b>")))),
              fluidRow(
                column(6,div(style = "height:10px;"), 
                       align="right",
                       conditionalPanel(
                         condition = "input.vote == '1'",
                         uiOutput("refresh"))),
                column(6, div(style = "height:10px;"), 
                       align="left",
                       conditionalPanel(
                         condition = "input.vote == '1'",
                         uiOutput("results")))
              )
      ),
      
      tabItem(tabName = "resu", 
              fluidRow(valueBoxOutput("no_votes", width=3)),
              fluidRow(highchartOutput("hc_results"))
      )
      
                )    
      )
  )
  )