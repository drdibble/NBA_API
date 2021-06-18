#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(formattable)
library(shinyWidgets)
library(shinydashboard)


# read in CSV with table information
df = read.csv('~/Desktop/App/official_free_agents.csv')
df1 = read.csv('~/Desktop/App/college.csv')
#df$Previous.Salary <- sub("^", "", df$Previous.Salary )

# #assign a class    
# class(df$Previous.Salary) <- c("money", class(df$Previous.Salary))
# 
# #S3 print method for the class    
# print.money <- function(x, ...) {
#   print.default(paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=",")))
# }
# 
# #format method, which is necessary for formating in a data.frame   
# format.money  <- function(x, ...) {
#   paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
# }

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Right Corner Scouting"),
  dashboardSidebar(sidebarMenu(
    #chart-bar, dribbble-square, horse-head, th, red-river
    menuItem("Court Visual", tabName = "court", icon = icon("basketball-ball")),
    menuItem("Right Corner Scouting", tabName = "scouting", icon = icon("bullseye")),
    menuItem("Probability Model", "model", icon = icon("horse-head"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "court",
              fluidPage(
                img(src = "court.jpeg", height = 800, width = 1150) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "model",
              fluidPage(
                img(src = "court.jpeg", height = 800, width = 1150) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "scouting",
              navbarPage("Players",
                         
                         #NBA free agent page
                         tabPanel(img(src = "nba.png", height = 60, width = 30),
                                  fluidRow(
                                    "2021 Free Agents"
                                  ),
                                  fluidRow(
                                    #Row for filters.
                                    
                                    #Position
                                    #Two options for multiple select: check boxes, or dropdown select.
                                    column(4,
                                           #Check boxes, uncomment to use.
                                           checkboxGroupInput(
                                             inputId = "pos",
                                             label = "Position:",
                                             choices = c(unique(as.character(df$Position))),
                                                          selected = c(unique(as.character(df$Position))),
                                             inline = TRUE
                                           ) #checkboxGroupInput
                                           #Dropdown select, uncomment to use.
                                           # selectInput(inputId = "pos",
                                           #             label = "Position:",
                                           #             choices = c(unique(as.character(df$Position))),
                                           #             selected = c(unique(as.character(df$Position))),
                                           #             multiple = TRUE) #selectInput
                                    ), #column
                                    
                                    #Free agent type
                                    column(4,
                                           checkboxGroupInput(
                                             inputId = "type",
                                             label = "Free Agent Type:",
                                             choices = c(unique(as.character(df$Type))),
                                             selected = c(unique(as.character(df$Type))),
                                             inline = TRUE
                                           ) #checkboxGroupInput
                                    ), #column
                                  ), #fluid row
                                  
                                  #Row for sliders
                                  fluidRow(  
                                    #Previous salary range
                                    column(4,
                                           sliderInput(inputId = "sal",
                                                       label = "Previous Salary Range",
                                                       min = 0,
                                                       max = 40000000,
                                                       value = c(0,40000000),
                                                       step = 1000000,
                                                       ticks = FALSE,
                                                       pre = '$'
                                           ) #sliderInput
                                    ), #column
                                    
                                    #Minimum attempts.
                                    column(4,
                                           sliderInput(inputId = "att",
                                                       label = "Minimum Attempts",
                                                       min = 0,
                                                       max = 124,
                                                       value = 0,
                                                       step = 1,
                                                       ticks = FALSE,
                                           ) #sliderInput
                                    )
                                  ),
                                  
                                  fluidRow("Note: The following table uses data from the entire 2020-21 season."),
                                  
                                  fluidRow(
                                    DTOutput("my_table", width = "100%"), #DT output
                                  )
                                  
                         ), #tabPanel, NBA free agents
                         
                         #College player page
                         tabPanel(img(src = "draft.png", height = 60, width = 60),
                                  fluidRow(
                                    "NBA Draft Prospects"
                                  ),
                                  fluidRow(
                                    #Row for filters.
                                    
                                    #Position
                                    #Two options for multiple select: check boxes, or dropdown select.
                                    column(4,
                                           #Check boxes, uncomment to use.
                                           checkboxGroupInput(
                                             inputId = "pos1",
                                             label = "Position:",
                                             choices = c(unique(as.character(df$Position))),
                                                         selected = c(unique(as.character(df$Position))),
                                             inline = TRUE
                                           ) #checkboxGroupInput
                                           #Dropdown select, uncomment to use.
                                           # selectInput(inputId = "pos",
                                           #             label = "Position:",
                                           #             choices = c(unique(as.character(df$Position))),
                                           #             selected = c(unique(as.character(df$Position))),
                                           #             multiple = TRUE) #selectInput
                                    ), #column
                                    
                                    #Draft projection
                                    column(4,
                                           checkboxGroupInput(
                                             inputId = "draft1",
                                             label = "Draft Projection:",
                                             choices = c("Early First Round", "Mid First Round", "Late First Round",
                                                         "Early Second Round", "Mid Second Round", "Late Second Round",
                                                         "Outside First Two Rounds"),
                                             selected = c("Early First Round", "Mid First Round", "Late First Round",
                                                          "Early Second Round", "Mid Second Round", "Late Second Round",
                                                          "Outside First Two Rounds"),
                                             inline = TRUE,
                                             width = '700px'
                                           ) #checkboxGroupInput
                                    ), #column
                                  ), #fluid row
                                  
                                  #New row for slider
                                  fluidRow(  
                                    #Minimum attempts.
                                    column(4,
                                           sliderInput(inputId = "att1",
                                                       label = "Minimum Attempts",
                                                       min = 0,
                                                       max = 33,
                                                       value = 0,
                                                       step = 1,
                                                       ticks = FALSE,
                                           ) #sliderInput
                                    )
                                  ),
                                  
                                  fluidRow("Note: The following table uses data from the entire 2020-21 season."),
                                  
                                  #New row for table
                                  fluidRow(
                                    DTOutput("my_table1", width = "100%"), #DT output
                                  )
                         ) #tabPanel, NBA free agents
              ) #navbarPage
      ) #tabItem
      
      
    ) #tabItems
    
    
  ) #dashboardBody
) #dashboardPage
  


# Define server
server <- function(input, output) {
  #Output for NBA panel
  output$my_table = DT::renderDataTable(
    DT::datatable(
      {data <- df
        data <- data[data$Position %in% input$pos,]
        data <- data[data$Type %in% input$type,]
        data <- data[(data$Previous.Salary <= input$sal[2]) &
                     (data$Previous.Salary >= input$sal[1]),]
        data <- data[data$FGA >= input$att,]
      data
    },
      escape = FALSE,
      rownames = FALSE,
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0)),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100)),
      selection = 'single'
    ) %>% #datatable
      formatCurrency(c("Previous.Salary"), "$")
  )#renderDataTable
  
  #Output for college panel
  output$my_table1 = DT::renderDataTable(
    DT::datatable(
      {data1 <- df1
      #if (input$pos != 'All') {
      data1 <- data1[data1$Position %in% input$pos1,]
      data1 <- data1[data1$Mock.Draft %in% input$draft1,]
      data1 <- data1[data1$FGA >= input$att1,]
      #}
      data1
      },
      escape = FALSE,
      rownames = FALSE,
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0)),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100)),
      selection = 'single'
    ) #datatable
  ) #renderDataTable
}

# Run the application
shinyApp(ui = ui, server = server)
