#' LMD Shiny
#'
#' @description
#' Shiny Dashboard for plotting Product Functions (PFs) and Residue
#'
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @keywords Shiny
#' @import ggplot2
#' @import patchwork
#' @import shiny
#' @import shinyjs
#' @examples
#' lmd_shiny()
#' @export lmd_shiny

lmd_shiny <- function(){



  # Define the shiny app function
  runShinyApp <- function() {

    # Define UI
    ui <- fluidPage(
      titlePanel("LMD: Data Visualization"),
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Choose a CSV file",
                    accept = c("text/csv", "text/comma-separated-values,text/plain",
                               ".csv")),
          selectInput("column", "Select a column", choices = NULL),
          actionButton("submit", "Submit")
        ),
        mainPanel(
          plotOutput("plot"),
          plotOutput("lmdplot")

        )
      )
    )

    # Define server
    server <- function(input, output, session) {

      # Load dataset
      dataset <- reactive({
        req(input$file)
        read.csv(input$file$datapath)
      })

      # Update column choices based on selected dataset
      observeEvent(dataset(), {
        updateSelectInput(session, "column", choices = names(dataset()))
      })

      # Generate plot when submit button is clicked
      observeEvent(input$submit, {
        output$plot <- renderPlot({
          # browser()
          req(input$column)
          ggplot(dataset(), aes_string(x = seq_len(nrow(dataset())), y = input$column)) +
            geom_line() +
            labs(x = "Index", y = input$column,title = "Original Signal")+
            theme_minimal() +
            theme(legend.position = "none")
        })
      })

      observeEvent(input$submit, {
        output$lmdplot <- renderPlot({
          # browser()
          req(input$column)
          plot_lmd(lmd(dataset()[input$column][,1]))


        })
      })

    }

    # Run the shiny app
    shinyApp(ui, server)
  }

  # Run the shiny app function
  runShinyApp()

}


