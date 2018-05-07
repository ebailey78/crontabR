logsOutput <- function(id) {
  ns <- NS(id)
  miniTabPanel("View Logs", icon = icon("table"),
               miniContentPanel(
                 DT::dataTableOutput(ns("table")),
                 tags$div(style = "position: absolute; right: 5px; bottom: 5px;",
                          actionButton(ns("update"), "Update Logs", icon = icon("refresh"))
                 )
               )
  )
}

logs <- function(input, output, session) {

  values <- reactiveValues(logs = getLog())

  # Reload the Log file every 30 seconds
  timer <- reactiveTimer(30000)
  observeEvent(timer(), {
    values$logs <- getLog()
  })

  observeEvent(input$update, {
    values$logs <- getLog()
  })

  output$table <- DT::renderDataTable({
    log <- values$logs
    log$job <- ifelse(nchar(log$job) > 23, paste0(substring(log$job, 1, 20), "..."), log$job)
    log$date <- as.POSIXct(log$date, format = "%Y-%m-%d %I:%M %p")
    return(log)
  },
    selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
    options = list(pageLength = 5, dom="tip"), filter = "top"
  )

}
