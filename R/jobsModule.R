viewOutput <- function(id) {

  ns <- NS(id)

  miniTabPanel("View Jobs", icon = icon("eye"),
               miniContentPanel(
                 DT::dataTableOutput(ns("table")),
                 tags$div(style = "position: absolute; right: 5px; bottom: 5px;",
                          actionButton(ns("run"), "Run Selected Job", icon = icon("play"))
                 )
               )
  )

}

view <- function(input, output, session, jobs) {

  observeEvent(input$run, {

    i = input$table_rows_selected

    cmd <- ""

    if(jobs$bashrc[i] == "TRUE") {
      cmd <- "source $HOME/.bashrc; "
    }

    cmd <- paste0(cmd, "Rscript ~/.crontabR/", jobs$cronjob[i], ".R")

    system(cmd, wait = FALSE)

  })

  output$table <- DT::renderDataTable({
    x <- jobs[, c("cronjob", "description", "interval", "nextRun", "logLevel")]
    x$cronjob <- ifelse(nchar(x$cronjob) > 23, paste0(substring(x$cronjob, 1, 20), "..."), x$cronjob)
    x$description <- ifelse(nchar(x$description) > 50, paste0(substring(x$description, 1, 47), "..."), x$description)
    colnames(x) <- c("Name", "Description", "Interval", "Next Run", "Log Level")
    x <- x[order(x$Name), ]
    return(x)
  },
  selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
  options = list(pageLength = 5, dom="tip"), filter = "top"
  )


}
