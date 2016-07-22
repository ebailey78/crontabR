createOutput <- function(id) {
  ns <- NS(id)

  tabPanel("Create/Update", icon = icon("magic"),
    htmlOutput(ns("status")),
    fixedRow(style = "margin: 10px;",
      column(width = 6,
        panel(title = "Select Cronjob", style = "default",
          selectInput(ns("currentJobs"), NULL, choices = c(), width = "100%")
        ),
        panel(title = "Scheduler", style = "default",
              miniColumn(width = 6,
                         dateInput(ns("startDate"), "Date", min = Sys.Date(), value = Sys.Date())
              ),
              miniColumn(width = 6,
                         textInput(ns("startTime"), "Time", value = format(Sys.time(), "%I:%M %p"), placeholder = "HH:MM _M")
              ),
              miniColumn(width = 6,
                         selectInput(ns("frequency"), label = NULL, choices = c("Hourly", "Daily", "Weekly", "Monthly", "Yearly"), selected = "Weekly")
              ),
              miniColumn(width = 4, style = "text-align: right;",
                         checkboxInput(ns("minuteJiggle"), "Jiggle Time")
              )
        ),
        panel(title = "Script Selection", style = "default",
              fileInput(ns("scriptUpload"), label = NULL)
        )
      ),
      column(width = 6,
        panel(title = "Options", style = "default",
              textInput(ns("cronjobName"), label = "Cronjob Name", width = "100%"),
              column(width = 6, style = "padding-left: 0;",
                selectInput(ns("cronlogLevel"), label = "Logging Level", width = "100%",
                            choices = c("error", "warn", "info", "verbose", "debug", "silly"),
                            selected = "info")
              ),
              column(width = 6, style = "padding-right: 0;",
                selectInput(ns("cronTextLevel"), label = "Text Level", width = "100%",
                            choices = c("none", "error", "warn", "info", "verbose", "debug", "silly"),
                            selected = "error"
                            )
              ),
              textAreaInput(ns("cronjobDesc"), rows = "2", "Cronjob Description"),
              textAreaInput(ns("environmentVariables"), rows = "2", "Environment Variables"),
              tags$div(class = "col-sm-6",
                       checkboxInput(ns("overwriteCronjob"), "Overwrite Cronjob", value = FALSE)
              ),
              tags$div(class = "col-sm-6",
                       checkboxInput(ns("sourceBashrc"), "Source .bashrc", value = TRUE)
              )
        )
      )
    ),
    tags$div(style = "width: 100%; padding: 20px; position: absolute; bottom: 0;", class = "btn-group btn-group-justified",
      tags$div(class = "btn-group",
             tags$button(id = ns("addCronjob"), class = "btn btn-default action-button btn-block", icon("plus"), "Add")
      ),
      tags$div(class = "btn-group",
               tags$button(id = ns("updateCronjob"), class = "btn btn-default action-button btn-block", icon("pencil"), "Update")
      ),
      tags$div(class = "btn-group",
               tags$button(id = ns("deleteCronjob"), class = "btn btn-default action-button btn-block", icon("minus"), "Delete")
      ),
      tags$div(class = "btn-group",
               tags$button(id = ns("clearForm"), class = "btn btn-default action-button btn-block", icon("ban"), "Clear")
      )
    ),
    tags$script('
      Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
        var id = "#" + x;
        var pb_id = "#" + x + "_progress";
        var idBar = pb_id + " .bar";
        $(id).val("");
        $(pb_id).css("visibility", "hidden");
        $(idBar).css("width", "0%");
      });
    ')
  )
}

create <- function(input, output, session) {

  ns <- session$ns

  values <- reactiveValues(status = status("Ready to Cron It Up!", "info"),
                           cronjobs = listCronjobs(),
                           updateType = "none")

  output$status <- renderText(values$status)

  observeEvent(input$currentJobs, {
    if(input$currentJobs %in% values$cronjobs$cronjob) {

      cj <- values$cronjobs[values$cronjobs$cronjob == input$currentJobs, ]

      updateTextInput(session, "cronjobName", value = cj$cronjob)
      updateTextInput(session, "cronjobDesc", value = cj$desc)
      updateSelectInput(session, "cronlogLevel", selected = cj$logLevel)
      updateSelectInput(session, "cronTextLevel", selected = cj$textLevel)
      updateTextInput(session, "environmentVariables", value = cj$envvar)

      updateDateInput(session, "startDate", value = as.Date(cj$nextRun))
      updateTextInput(session, "startTime", value = format(as.POSIXct(cj$nextRun, format = dateTimeFormat), format = "%I:%M %p"))

      updateSelectInput(session, "frequency", selected = cj$interval)

      updateCheckboxInput(session, "minuteJiggle", value = FALSE)

      session$sendCustomMessage(type = "resetFileInputHandler", "scriptUpload")
      values$status <- status("Ready to Cron It Up!", "info")

    }
  })

  datetime <- reactive({

    time <- as.POSIXct(paste(input$startDate, input$startTime), format = "%Y-%m-%d %I:%M %p")

    if(input$minuteJiggle) {
      j <- as.integer(runif(1, 1, 15)) * 60
      time <- time + j
    }

    dt <- list(
      M = as.integer(format(time, "%M")),
      H = as.integer(format(time, "%H")),
      d = as.integer(format(time, "%d")),
      m = as.integer(format(time, "%m")),
      W = as.integer(as.POSIXlt(time)$wday)
    )

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

  jobCheck <- function() {
    ok <- TRUE
    if(!is.null(input$scriptUpload)) {
      if(tools::file_ext(input$scriptUpload$name[1]) != "R") {
        values$status <- status("The script file should have a .R extension.", "danger")
        ok <- FALSE
      }
    } else {
      if(values$updateType == "add") {
        values$status <- status("You must include a script file.", "danger")
        ok <- FALSE
      }
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

  # Ensure proper formatting of job name
  observeEvent(input$cronjobName, {
    updateTextInput(session, "cronjobName", value = formatNames(input$cronjobName))
  })

  # Keeps the "Select Cronjob" list up-to-date
  observeEvent(values$cronjobs, {
    ccj <- input$currentJobs
    if(!ccj %in% values$cronjobs$cronjob) ccj <- NULL
    updateSelectInput(session, "currentJobs", choices = c("", values$cronjobs$cronjob), selected = ccj)
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

  observeEvent(input$scriptUpload, {
    if(tools::file_ext(input$scriptUpload$name) != "R") {
      values$status = status("The uploaded file does not have a .R extension", "warning")
    } else {
      values$status = status("Ready to Cron it up!", "info")
    }
  })

  # Add Cronjob
  observeEvent(input$addCronjob, {

    values$updateType <- "add"

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
        bashrc = input$sourceBashrc,
        warn=TRUE,
        logLevel = input$cronlogLevel,
        textLevel = input$cronTextLevel
      )) {
        values$cronjobs <- listCronjobs()
        values$status <- status("Cronjob added!", "success")
      } else {
        values$status <- status("Unable to added cronjob!", "danger")
      }
    }
    values$updateType <- "none"


  })

  # Update Cronjob
  observeEvent(input$updateCronjob, {

    values$updateType <- "update"

    if(jobCheck()) {

      if(input$overwriteCronjob) {

        if(is.null(input$scriptUpload)) {
          script_loc <- tempfile(fileext = ".R")
          file.copy(paste0(script_directory, input$currentJobs, ".R"), script_loc)
        } else {
          script_loc <- input$scriptUpload$datapath
        }

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
          script_path = script_loc,
          overwrite = input$overwriteCronjob,
          warn=TRUE,
          logLevel = input$cronlogLevel,
          textLevel = input$cronTextLevel
        )) {
          values$cronjobs <- listCronjobs()
          values$status <- status("Cronjob updated.", "success")
        }
      } else {
        values$status <- status("You must check \"Overwrite Cronjob\" to update.", "warning")
      }
    }
    values$updateType <- "none"

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

  clearForm <- function() {

    updateSelectInput(session, "currentJobs", selected = "")

    updateTextInput(session, "cronjobName", value = "")
    updateTextInput(session, "cronjobDesc", value = "")
    updateTextInput(session, "environmentVariables", value = "")

    updateDateInput(session, "startDate", value = Sys.Date())
    updateTextInput(session, "startTime", value = format(Sys.time(), "%I:%M %p"))

    updateSelectInput(session, "cronlobLevel", selected = "info")
    updateSelectInput(session, "cronTextLevel", selected = "none")

    updateSelectInput(session, "frequency", selected = "Weekly")
    session$sendCustomMessage(type = "resetFileInputHandler", session$ns("scriptUpload"))

  }

  observeEvent(input$clearForm, {

    clearForm()
    values$status <- status("Ready to Cron It Up!", "info")

  })

}
