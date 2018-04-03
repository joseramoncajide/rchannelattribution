#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(rchannelattribution)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Old Faithful Geyser Data"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        fileInput("upload", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv",
                             ".zip"))
        ,
        actionButton("process", "Process uploaded data")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput("models")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # tmp_dir <- file.path(getwd(), 'tmp')
  #
  # model_data <- reactive({
  #   tmp_dir <- file.path(getwd(), 'tmp')
  #   cat(input$upload$datapath,"\n")
  #   all_files <- unzip(zipfile = input$upload$datapath, exdir = tmp_dir)
  #   all_files <- list.files('tmp', full.names = T)
  #   csv_files <- list.files('tmp', full.names = T, pattern = "*.csv")
  #   unlink(setdiff(all_files, csv_files),recursive = T)
  #   import_path <- tmp_dir
  #   attribution_res <- data_driven_model(import_path)
  # })



  model_data <- eventReactive(input$process, {
    tmp_dir <- file.path(getwd(), 'tmp')
    cat(input$upload$datapath,"\n")
    all_files <- unzip(zipfile = input$upload$datapath, exdir = tmp_dir)
    all_files <- list.files('tmp', full.names = T)
    csv_files <- list.files('tmp', full.names = T, pattern = "*.csv")
    unlink(setdiff(all_files, csv_files),recursive = T)
    import_path <- tmp_dir
    attribution_res <- data_driven_model(import_path)
  })

  output$models <- renderDataTable({

    req(model_data())

    validate(
      need(model_data(), "getting user details")
    )

    model_data()$data

  }, options = list(paging = FALSE,searching = FALSE))
}

# Run the application
shinyApp(ui = ui, server = server)

