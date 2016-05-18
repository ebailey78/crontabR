library(shiny)
library(miniUI)
library(DT)

textAreaInput <- function (inputId, label, value = "", rows = 3, placeholder = NULL)
{
  tags$div(class = "form-group shiny-input-container", style = "width: 100%;",
           tags$label(`for` = inputId, label),
           tags$textarea(id = inputId, rows = rows, width = "100%", class = "form-control")
  )
}

panel <- function(..., title, style = "default") {

  tags$div(class = paste0("panel panel-", style),
           style = "margin-top: 10px;",
    tags$div(class = "panel-heading",
      tags$h3(class = "panel-title", title)
    ),
    tags$div(class = "panel-body", style = "padding-bottom: 0;",
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
    miniTabstripPanel(
      tabPanel("Create", icon = icon("magic"),
        column(width = 7,
          panel(title = "Select Cronjob", style = "default",
            selectInput("currentJobs", NULL, choices = c(), width = "100%")
          ),
          panel(title = "Options", style = "default",
            textInput("cronjobName", label = "Cronjob Name", width = "100%"),
            textAreaInput("cronjobDesc", rows = "2", "Cronjob Description"),
            textAreaInput("environmentVariables", "Environment Variables"),
            checkboxInput("overwriteCronjob", "Overwrite Existing Cronjob?", value = FALSE)
          )
        ),
        column(width = 5,
               panel(title = "Scheduler", style = "default",
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
               ),
               tags$div(class = "btn-group",
                  tags$button(id = "addCronjob", class = "btn btn-success action-button", icon("plus"), "Add"),
                  tags$button(id = "updateCronjob", class = "btn btn-warning action-button", icon("pencil"), "Update"),
                 tags$button(id = "deleteCronjob", class = "btn btn-danger action-button", icon("minus"), "Delete"),
                 tags$button(id = "clearAddForm", class = "btn btn-info action-button", icon("ban"), "Clear")
               )


        )
      ),
      miniTabPanel("View", icon = icon("eye"),
        miniContentPanel(
          DT::dataTableOutput("cronJobTable")
        )
      ),
      miniTabPanel("Logs", icon = icon("table"),
        miniContentPanel(
          DT::dataTableOutput("logs")
        )
      )#,
      # miniTabPanel("Options", icon = icon("wrench"),
      #   miniContentPanel(
      #     "Test Options"
      #   )
      # )
    )
  )

  server <- function(input, output, session) {

    values <- reactiveValues(cronjobs = listCronjobs())

####### REACTIVES ########

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

    # Keeps the "Select Cronjob" list up-to-date
    observeEvent(values$cronjobs, {
      ccj <- input$currentJobs
      if(!ccj %in% values$cronjobs$cronjob) ccj <- NULL
      updateSelectInput(session, "currentJobs", choices = c("", values$cronjobs$cronjob), selected = ccj)
    })

    # Creates the CronString for use with addCronjob
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
        },
        "Yearly" = {
          dt[5] = "*"
        }
      )

      paste(dt, collapse = "\t")

    })

    # Formats the environmental variables
    envVar <- reactive({
      vars <- strsplit(input$environmentVariables, "\n", fixed = TRUE)[[1]]
      vars <- vars[grep("=", vars, fixed = TRUE)]
      vars <- strsplit(vars, "=", fixed = TRUE)
      n <- sapply(vars, function(x) trimws(x[[1]]))
      vars <- sapply(vars, function(x) trimws(x[[2]]))
      names(vars) <- n
      vars
    })

    ##### Observers

    # Ensure proper formatting of job name
    observeEvent(input$cronjobName, {
      updateTextInput(session, "cronjobName", value = formatNames(input$cronjobName))
    })

    # Populate cronjob info
    observeEvent(input$currentJobs, {

      if(input$currentJobs %in% values$cronjobs$cronjob) {
        cj <- values$cronjobs[values$cronjobs$cronjob == input$currentJobs, ]

        updateTextInput(session, "cronjobName", value = cj$cronjob)
        updateTextInput(session, "cronjobDesc", value = cj$desc)
        updateTextInput(session, "environmentVariables", value = cj$envvar)

        updateDateInput(session, "startDate", value = as.Date(cj$nextRun))
        updateTextInput(session, "startTime", value = format(as.POSIXct(cj$nextRun, format = dateTimeFormat), format = "%I:%M %p"))

        updateSelectInput(session, "frequency", selected = cj$interval)
      }

    })

    jobCheck <- function() {
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

      return(ok)
    }

    clearForm <- function() {

      updateSelectInput(session, "currentJobs", selected = "")

      updateTextInput(session, "cronjobName", value = "")
      updateTextInput(session, "cronjobDesc", value = "")
      updateTextInput(session, "environmentVariables", value = "")

      updateDateInput(session, "startDate", value = Sys.Date())
      updateTextInput(session, "startTime", value = format(Sys.time(), "%I:%M %p"))

      updateSelectInput(session, "frequency", selected = "Weekly")

    }

    # Add Cronjob
    observeEvent(input$addCronjob, {


      if(jobCheck()) {

        if(input$cronjobDesc == "") {
          desc <- NULL
        } else {
          desc <- input$cronjobDesc
        }

        if(addCronjob(
          name = input$cronjobName,
          desc = desc,
          env_vars = envVar(),
          scheduled_time = cronString(),
          script_path = input$scriptUpload$datapath,
          overwrite = input$overwriteCronjob,
          warn=TRUE
        )) {
          values$cronjobs <- listCronjobs()
        }
      }


    })

    # Update Cronjob
    observeEvent(input$updateCronjob, {

      if(jobCheck()) {
        if(input$overwriteCronjob) {
          deleteCronjob(input$currentJobs)

          if(input$cronjobDesc == "") {
            desc <- NULL
          } else {
            desc <- input$cronjobDesc
          }

          if(addCronjob(
            name = input$cronjobName,
            desc = desc,
            env_vars = envVar(),
            scheduled_time = cronString(),
            script_path = input$scriptUpload$datapath,
            overwrite = input$overwriteCronjob,
            warn=TRUE
          )) {
            values$cronjobs <- listCronjobs()
          }
        }
      }

    })

    observeEvent(input$deleteCronjob, {

      if(input$overwriteCronjob) {
        deleteCronjob(input$currentJobs)
        clearForm()
        values$cronjobs <- listCronjobs()
      }

    })

    observeEvent(input$clearForm, {
      clearForm()
    })

    output$logs <- DT::renderDataTable({getLog()},
      selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
      options = list(pageLength = 7, dom="tip")
    )

    output$cronJobTable <- DT::renderDataTable({values$cronjobs[, -ncol(values$cronjobs)]},
      selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
      options = list(pageLength = 7, dom="tip")
    )

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer("crontabR", width = 800, height = 600)

  runGadget(ui, server, viewer = viewer)

}

#crontabAddin()
