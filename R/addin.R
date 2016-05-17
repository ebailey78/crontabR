library(shiny)
library(miniUI)
library(DT)

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

crontabRAddin <- function() {

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
            ),
            checkboxInput("overwriteCronjob", "Overwrite Existing Cronjob?", value = FALSE)
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
      # miniTabPanel("Modify", icon = icon("pencil"),
      #   miniContentPanel(
      #     "Test Modify"
      #   )
      # ),
      miniTabPanel("Logs", icon = icon("table"),
        miniContentPanel(
          DT::dataTableOutput("logs")
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

      d <- as.POSIXlt(input$startDate)
      t <- as.POSIXlt(input$startTime, format = "%I:%M %p")

      dt <- list(
        M = as.integer(format(t, "%M")),
        H = as.integer(format(t, "%H")),
        d = as.integer(format(d, "%d")),
        m = as.integer(format(d, "%m")),
        W = as.integer(as.POSIXlt(d)$wday)
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

    envVar <- reactive({
      vars <- strsplit(input$environmentVariables, "\n", fixed = TRUE)[[1]]
      vars <- vars[grep("=", vars, fixed = TRUE)]
      vars <- strsplit(vars, "=", fixed = TRUE)
      n <- sapply(vars, function(x) trimws(x[[1]]))
      vars <- sapply(vars, function(x) trimws(x[[2]]))
      names(vars) <- n
      vars
    })

    observeEvent(input$cronjobName, {
      updateTextInput(session, "cronjobName", value = formatNames(input$cronjobName))
    })

    observeEvent(input$addCronjob, {

      ok <- TRUE
      if(!is.null(input$scriptUpload)) {
        if(tools::file_ext(input$scriptUpload$name[1]) != "R") {
          ok <- FALSE
        }
      } else {
        ok <- FALSE
      }

      if(is.null(input$cronjobName) | input$cronjobName == "") {
        ok <- FALSE
      }

      if(is.na(as.POSIXct(input$startTime, format = "%I:%M %p"))) {
        ok <- FALSE
      }

      if(ok) {
        addCronjob(input$cronjobName, envVar(), cronString(), input$scriptUpload$datapath, overwrite = input$overwriteCronjob, warn=TRUE)
      }


    })

    output$logs <- DT::renderDataTable({getLog()}, server = FALSE)

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer("crontabR", width = 800, height = 600)

  runGadget(ui, server, viewer = viewer)

}

#crontabAddin()
