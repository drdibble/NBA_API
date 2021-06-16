#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Not run: ------------------------------------
# table example
# shinyApp(
#   ui = fluidPage(
#     fluidRow(
#       column(12,
#         tableOutput('table')
#       )
#     )
#   ),
#   server = function(input, output) {
#     output$table <- renderTable(iris)
#   }
# )

library(shiny)
library(DT)
library(formattable)

# read in CSV with table information
df = read.csv('free_agents.csv')
df$Previous.Salary <- currency(df$Previous.Salary)
head(df)

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
ui <- fluidPage(
  fluidRow(
    DTOutput("my_table", width = "100%")
  )
)

# Define server
server <- function(input, output) {
  output$my_table = DT::renderDataTable(
    DT::datatable(
      {data <- df},
      escape = FALSE,
      rownames = FALSE,
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0)),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100)),
      selection = 'single'
    )#datatable
  )#renderDataTable
}

# Run the application
shinyApp(ui = ui, server = server)
# # DataTables example
# shinyApp(
#   ui = fluidPage(
#     fluidRow(
#       column(12,
#         dataTableOutput('table')
#       )
#     )
#   ),
#   server = function(input, output) {
#     college_players <- read.csv(file = 'college.csv')
#     output$table <- renderDataTable(college_players)
#   }
# )
# ---------------------------------------------