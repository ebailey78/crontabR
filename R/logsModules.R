logsUI <- function(id) {

  ns <- NS(id)

  miniContentPanel(
    DT::dataTableOutput(ns("logs")),
    tags$div(style = "position: absolute; right: 5px; bottom: 5px;",
             actionButton(ns("updateLogs"), "Update Logs", icon = icon("refresh"))
    )
  )

}

logs <- function(input, output, session, pageLength = 5, dom = "tip", filter = "top") {

  values <- reactiveValues(logs = crontabR::getLog())

  observeEvent(input$updateLogs, {values$logs <- getLog()})

  output$logs <- DT::renderDataTable({
    log <- values$logs
    log$job <- ifelse(nchar(log$job) > 23, paste0(substring(log$job, 1, 20), "..."), log$job)
    log$date <- as.POSIXct(log$date, format = "%Y-%m-%d %I:%M %p")
    return(log)
  },
  selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
  options = list(pageLength = pageLength, dom=dom), filter = filter
  )

}
