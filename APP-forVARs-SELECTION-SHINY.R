library(shiny)
library(kableExtra)
library(tidyverse)
library(modelsummary)
library(corrplot)
library(correlation)
library(psych)
library(shiny)
library(kableExtra)
library(report)
library(easystats)

#The code imports several R packages required for the analysis.

ui <- fluidPage(
     titlePanel("Interactive Variable Selection"),
     
     sidebarLayout(
          sidebarPanel(
               h3("Select your Variables"),
               actionButton("select_vars", "Select Variables")
          ),
          
          mainPanel(
               h3("Summary"),
               verbatimTextOutput("summary"),
               
               h3("Correlation Test"),
               verbatimTextOutput("corr_test"),
               
               h3("Report"),
               verbatimTextOutput("report")
          )
     )
)

# This code creates a basic UI using the Shiny package in R
# The UI consists of a title panel and a sidebar layout with a sidebar panel and a main panel
# The sidebar panel has an action button named 'select_vars'
# The main panel includes three sections for output: 'summary', 'corr_test', and 'report'
# The output sections will be displayed as verbatim text output

server <- function(input, output, session) {
     selected_vars <- reactiveVal()
     
     iris2 <- iris %>%
          rename(SW = Sepal.Width,
                 PL = Petal.Length,
                 PW = Petal.Width)
     
     observeEvent(input$select_vars, {
          vars <- select.list(names(iris2), multiple = TRUE,
                              title = 'Select your Variables',
                              graphics = TRUE)
          selected_vars(vars)
     })
     
     output$summary <- renderPrint({
          req(selected_vars())
          newiris <- iris2[, c(selected_vars())]
          summary(newiris)
     })
     
     output$corr_test <- renderPrint({
          req(selected_vars())
          newiris <- iris2[, c(selected_vars())]
          corr.test(newiris)
     })
     
     output$report <- renderPrint({
          req(selected_vars())
          newiris <- iris2[, c(selected_vars())]
          report(newiris)
     })
}
# This is a server function for a Shiny app. 
# 
# The `iris` dataset is loaded, and then its column names are renamed in the `iris2` data frame.
# 
# `selected_vars` is a reactive value used for storing user-selected variables.
# 
# `observeEvent` is used to create a pop-up window with a list of variable names from the `iris2` data frame when the `input$select_vars` is triggered. The selected variables are then stored in `selected_vars`.
# 
# There are three `output` functions: 
# - `output$summary` prints the summary of the `iris2` data frame including only selected variables. 
# - `output$corr_test` prints the results of the correlation test among the selected variables of `iris2`. 
# - `output$report` prints a report of the data including global information about the variables and their distribution, as well as pairwise comparisons of variables.
# 
# All of these functions require that the user has selected variables, as they call `req(selected_vars())`.


shinyApp(ui = ui, server = server)
# This code represents the creation and launching of a Shiny web application. `ui` refers to the user interface of the app, and `server` refers to the logic that handles user input and produces output for the interface. This code should be used at the end of the script after both `ui` and `server` have been defined.
