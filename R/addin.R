library(shiny)
library(miniUI)
library(shinydashboard)

panel <- function(..., title, style = "default") {

  tags$div(class = paste0("panel panel-", style),
           style = "margin-top: 10px;",
    tags$div(class = "panel-heading",
      tags$h3(class = "panel-title", title)
    ),
    tags$div(class = "panel-body",
      ...
    )
  )
}

miniColumn <- function(width, ..., offset = 0) {
  if (!is.numeric(width) || (width < 1) || (width > 12))
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width)
  if (offset > 0)
    colClass <- paste0(colClass, " col-sm-offset-", offset)
  div(class = colClass, ..., style = "padding: 0;")
}

crontabAddin <- function() {

  ui <- miniPage(
#    miniTitleBar("crontabR Gadget", right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniTabstripPanel(
      tabPanel("Create", icon = icon("plus"),
        column(width = 5,
          panel(title = "Run Scheduler", style = "default",
            miniColumn(width = 6,
              dateInput("startDate", "Date", min = Sys.Date(), value = Sys.Date())
            ),
            miniColumn(width = 6,
              textInput("startTime", "Time", value = format(Sys.time(), "%I:%M %p"), placeholder = "HH:MM _M")
            ),
            miniColumn(width = 12,
              selectInput("frequency", label = NULL, choices = c("Hourly", "Daily", "Weekly", "Monthly", "Yearly"), selected = "Weekly")
            )#,
            # tags$div(style = "text-align: center; width: 100%", "- OR -"),
            # miniColumn(width = 12,
            #   textInput("cronString", "Custom CronTab String", placeholder = "* * * *")
            # )
          ),
          panel(title = "Script Selection", style = "default",
            fileInput("scriptUpload", label = NULL)
          )
        ),
        column(width = 7,
          panel(title = "Options", style = "default",
            textInput("cronjobName", label = "Cronjob Name", width = "100%"),
            tags$div(class = "form-group shiny-input-container", style = "width: 100%;",
              tags$label(`for` = "environmentVariables", "Environment Variables"),
              tags$textarea(id = "environmentVariables", rows = "7", width = "100%", class = "form-control"),
              tags$small("Separate varibles with linebreaks, don't use quotes")
            )
          )
        ),
        column(width = 12,
          tags$button(id = "addCronjob", class = "btn btn-primary btn-block action-button", icon("check"), "Add CronJob"),
          tags$button(id = "clearAddForm", class = "btn btn-danger btn-block action-button", icon("ban"), "Clear Form")
        )
      ),
      miniTabPanel("Destroy", icon = icon("minus"),
        miniContentPanel(
          "Test Destroy"
        )
      ),
      miniTabPanel("Modify", icon = icon("pencil"),
        miniContentPanel(
          "Test Modify"
        )
      ),
      miniTabPanel("Logs", icon = icon("table"),
        miniContentPanel(
          "Test Logs"
        )
      ),
      miniTabPanel("Options", icon = icon("wrench"),
        miniContentPanel(
          "Test Options"
        )
      )
    )
  )

  server <- function(input, output, session) {

    datetime <- reactive({

      d <- as.POSIXct(input$startDate)
      t <- as.POSIXct(input$startTime, format = "%I:%M %p")

      dt <- list(
        M = format(t, "%M"),
        H = format(t, "%H"),
        d = format(d, "%d"),
        m = format(d, "%m"),
        W = as.POSIXlt(d)$wday
      )

    })

    cronString <- reactive({

      dt <- datetime()
      f <- input$frequency

      switch(input$frequency,

        "Hourly" = {
          dt[3:5] <- "*"
        },
        "Daily" = {
          dt[4:5] <- "*"
        },
        "Weekly" = {
          dt[3:4] <- "*"
        },
        "Monthly" = {
          dt[4:5] <- "*"
        }
      )

      paste(dt, collapse = "\t")

    })

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer("crontabR", width = 800, height = 525)

  runGadget(ui, server, viewer = viewer)

}

crontabAddin()
