status <- function(status, style = "info") {
  as.character(tags$div(class = paste0("alert alert-", style), style = "margin-top: 10px;",
          tags$strong("Status: "), status
  ))
}

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

#'@export
crontabRAddin <- function() {

  library(crontabR)

  ui <- miniPage(
    miniTabstripPanel(id = "whichTab",
      tabPanel("Create", icon = icon("magic"),
        column(width = 7,
          panel(title = "Select Cronjob", style = "default",
            selectInput("currentJobs", NULL, choices = c(), width = "100%")
          ),
          panel(title = "Options", style = "default",
            textInput("cronjobName", label = "Cronjob Name", width = "100%"),
            textAreaInput("cronjobDesc", rows = "2", "Cronjob Description"),
            textAreaInput("environmentVariables", "Environment Variables"),
            tags$div(class = "col-sm-6",
              checkboxInput("overwriteCronjob", "Overwrite Cronjob", value = FALSE)
            ),
            tags$div(class = "col-sm-6",
              checkboxInput("sourceBashrc", "Source .bashrc", value = TRUE)
            )
          )
        ),
        column(width = 5,
               panel(title = "Scheduler", style = "default",
                     miniColumn(width = 12,
                                selectInput("frequency", label = NULL, choices = c("Hourly", "Daily", "Weekly", "Monthly", "Yearly"), selected = "Weekly")
                     ),
                     miniColumn(width = 6,
                                dateInput("startDate", "Date", min = Sys.Date(), value = Sys.Date())
                     ),
                     miniColumn(width = 6,
                                textInput("startTime", "Time", value = format(Sys.time(), "%I:%M %p"), placeholder = "HH:MM _M")
                     ),
                     miniColumn(width = 12,
                                checkboxInput("minuteJiggle", "Jiggle")
                     )
               ),
               panel(title = "Script Selection", style = "default",
                     fileInput("scriptUpload", label = NULL)
               ),
               tags$div(class = "btn-group btn-group-justified", role = "group",
                tags$div(class = "btn-group",
                  tags$button(id = "addCronjob", class = "btn btn-success action-button", icon("plus"), "Add")
                ),
                tags$div(class = "btn-group",
                 tags$button(id = "updateCronjob", class = "btn btn-warning action-button", icon("pencil"), "Update")
                )
               ),
               tags$div(class = "btn-group btn-group-justified", style = "margin-top: 5px;",
                tags$div(class = "btn-group",
                 tags$button(id = "deleteCronjob", class = "btn btn-danger action-button", icon("minus"), "Delete")
                ),
                tags$div(class = "btn-group",
                 tags$button(id = "clearAddForm", class = "btn btn-info action-button", icon("ban"), "Clear")
                )
               ),
               htmlOutput("status")



        )
      ),
      miniTabPanel("View", icon = icon("eye"),
        miniContentPanel(
          actionButton("runJob", "Run Job Now"),
          DT::dataTableOutput("cronJobTable")
        )
      ),
      miniTabPanel("Logs", icon = icon("table"),
        miniContentPanel(
          tags$div(class = "col-sm-4",
            selectInput("logLevel", label = "Level", multiple = TRUE, choices = c())
          ),
          tags$div(class = "col-sm-4",
            selectInput("logJob", label = "Job", choices = c())
          ),
          tags$div(class = "col-sm-4",
            actionButton("updateLogs", "Update Logs")
          ),
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

    values <- reactiveValues(cronjobs = listCronjobs(),
                             status = status("Ready to Cron It Up!", "info"),
                             logs = getLog())

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

    observeEvent(input$whichTab, {
      print(input$whichTab)

    })

    observeEvent(input$runJob, {
      source(paste0("~/.crontabR/",
                   values$cronjobs$cronjob[input$cronJobTable_rows_selected],
                   ".R"))
    })

    observeEvent(input$frequency, {
      now <- Sys.time()
      freq <- input$frequency
      then <- switch(freq,
        Hourly = now,
        Daily = now,
        Weekly = now + weeks(1),
        Monthly = now + months(1),
        Yearly = now + years(1)
      )
      updateDateInput(session, "startDate", min = now, max = then)

    })

    # Creates the CronString for use with addCronjob
    cronString <- reactive({

      dt <- datetime()
      f <- input$frequency

      switch(input$frequency,

        "Hourly" = {
          dt[2:5] <- "*"
        },
        "Daily" = {
          dt[3:5] <- "*"
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

    observeEvent(input$updateLogs, {values$logs <- getLog()})

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
          values$status <- status("The script file should have a .R extension.", "danger")
          ok <- FALSE
        }
      } else {
        values$status <- status("You must include a script file.", "danger")
        ok <- FALSE
      }

      if(is.null(input$cronjobName) | input$cronjobName == "") {
        values$status <- status("You must provide a name for this job.", "danger")
        ok <- FALSE
      }

      if(is.null(input$startDate)) {
        values$status <- status("You must provide a valid date.", "danger")
      }

      if(is.na(as.POSIXct(input$startTime, format = "%I:%M %p"))) {
        values$status <- status("Time not correctly formatted.", "danger")
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

        if(input$minuteJiggle) {

          time <- as.POSIXct(paste(input$startDate, input$startTime), format = "%Y-%m-%d %I:%M %p")
          j <- as.integer(runif(1, 1, 15)) * 60
          time <- time + j

          updateDateInput(session, "startDate", value = format(time, format = "%Y-%m-%d"))
          updateTextInput(session, "startTime", value = format(time, format = "%I:%M %p"))

        }

        if(addCronjob(
          name = input$cronjobName,
          desc = desc,
          env_vars = envVar(),
          scheduled_time = cronString(),
          script_path = input$scriptUpload$datapath,
          overwrite = input$overwriteCronjob,
          bashrc = input$sourceBashrc,
          warn=TRUE
        )) {
          values$cronjobs <- listCronjobs()
          values$status <- status("Cronjob added!", "success")
        } else {
          values$status <- status("Unable to added cronjob!", "danger")
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
            values$status <- status("Cronjob updated.", "success")
          }
        } else {
          values$status <- status("You must check \"Overwrite Cronjob\" to update.", "warning")
        }
      }

    })

    observeEvent(values$logs, {
      updateSelectInput(session, "logLevel", choices = c(unique(values$logs$level)))
      updateSelectInput(session, "logJob", choices = c("all", unique(values$logs$job)))
    })

    observeEvent(input$deleteCronjob, {

      if(input$overwriteCronjob) {
        deleteCronjob(input$currentJobs)
        clearForm()
        values$cronjobs <- listCronjobs()
        values$status <- status("Cronjob deleted.", "success")
      } else {
        values$status <- status("You must check \"Overwrite Cronjob\" to delete.", "warning")
      }

    })

    observeEvent(input$clearAddForm, {
      clearForm()
    })

    output$logs <- DT::renderDataTable({
      log <- values$logs
      if(!is.null(input$logLevel)) {
        log <- log[log$level %in% input$logLevel, ]
      }
      if(input$logJob != "all") {
        log <- log[log$job %in% input$logJob, ]
      }
      log
    },
      selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
      options = list(pageLength = 9, dom="tp")
    )

    output$cronJobTable <- DT::renderDataTable({
        x <- values$cronjobs[, c("cronjob", "description", "interval", "nextRun")]
        colnames(x) <- c("Name", "Description", "Interval", "Next Run")
        return(x)
      },
      selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
      options = list(pageLength = 7, dom="tip")
    )

    output$status <- renderText(values$status)

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer("crontabR", width = 800, height = 600)


  runGadget(ui, server, viewer = viewer)

}

#crontabAddin()
