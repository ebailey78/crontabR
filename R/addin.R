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
            selectInput("cronlogLevel", label = "Logging Level", width = "100%",
                        choices = c("error", "warn", "info", "verbose", "debug", "silly"),
                        selected = "info"),
            textAreaInput("cronjobDesc", rows = "2", "Cronjob Description"),
            textAreaInput("environmentVariables", rows = "2", "Environment Variables"),
            tags$div(class = "col-sm-6",
              checkboxInput("overwriteCronjob", "Overwrite Cronjob", value = FALSE)
            ),
            tags$div(class = "col-sm-6",
              checkboxInput("sourceBashrc", "Source .bashrc", value = TRUE)
            )
          )
        ),
        column(width = 5,
               htmlOutput("status"),
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
               column(width = 6, style = "padding-left: 0; padding-right: 5px;",
                 tags$button(id = "addCronjob", class = "btn btn-success action-button btn-block", icon("plus"), "Add")
               ),
               column(width = 6, style = "padding-left: 5px; padding-right: 0;",
                 tags$button(id = "updateCronjob", class = "btn btn-warning action-button btn-block", icon("pencil"), "Update")
               ),
               column(width = 6, style = "padding-left: 0; padding-right: 5px; padding-top: 10px;",
                 tags$button(id = "deleteCronjob", class = "btn btn-danger action-button btn-block", icon("minus"), "Delete")
               ),
               column(width = 6, style = "padding-left: 5px; padding-right: 0; padding-top: 10px;",
                 tags$button(id = "clearAddForm", class = "btn btn-info action-button btn-block", icon("ban"), "Clear")
               )
        )
      ),
      miniTabPanel("View", icon = icon("eye"),
        miniContentPanel(
          DT::dataTableOutput("cronJobTable"),
          tags$div(style = "position: absolute; right: 5px; bottom: 5px;",
            actionButton("runJob", "Run Selected Job", icon = icon("play"))
          )
        )
      ),
      miniTabPanel("Logs", icon = icon("table"),
        logsUI("logTab")
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

  server <- function(input, output, session) {

    values <- reactiveValues(cronjobs = listCronjobs(),
                             status = status("Ready to Cron It Up!", "info"),
                             updateType = "none")

####### REACTIVES ########

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

    # Keeps the "Select Cronjob" list up-to-date
    observeEvent(values$cronjobs, {
      ccj <- input$currentJobs
      if(!ccj %in% values$cronjobs$cronjob) ccj <- NULL
      updateSelectInput(session, "currentJobs", choices = c("", values$cronjobs$cronjob), selected = ccj)
    })

    observeEvent(input$runJob, {

      i = input$cronJobTable_rows_selected

      cmd <- ""

      if(values$cronjobs$bashrc[i] == "TRUE") {
        cmd <- "source $HOME/.bashrc; "
      }

      cmd <- paste0(cmd, "Rscript ~/.crontabR/", values$cronjobs$cronjob[i], ".R")

      system(cmd, wait = FALSE)

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

    # Populate cronjob info
    observeEvent(input$currentJobs, {

      if(input$currentJobs %in% values$cronjobs$cronjob) {
        cj <- values$cronjobs[values$cronjobs$cronjob == input$currentJobs, ]

        updateTextInput(session, "cronjobName", value = cj$cronjob)
        updateTextInput(session, "cronjobDesc", value = cj$desc)
        updateSelectInput(session, "cronlogLevel", selected = cj$logLevel)
        updateTextInput(session, "environmentVariables", value = cj$envvar)

        updateDateInput(session, "startDate", value = as.Date(cj$nextRun))
        updateTextInput(session, "startTime", value = format(as.POSIXct(cj$nextRun, format = dateTimeFormat), format = "%I:%M %p"))

        updateSelectInput(session, "frequency", selected = cj$interval)

        updateCheckboxInput(session, "minuteJiggle", value = FALSE)

        session$sendCustomMessage(type = "resetFileInputHandler", "scriptUpload")
        values$status <- status("Ready to Cron It Up!", "info")
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

    clearForm <- function() {

      updateSelectInput(session, "currentJobs", selected = "")

      updateTextInput(session, "cronjobName", value = "")
      updateTextInput(session, "cronjobDesc", value = "")
      updateTextInput(session, "environmentVariables", value = "")

      updateDateInput(session, "startDate", value = Sys.Date())
      updateTextInput(session, "startTime", value = format(Sys.time(), "%I:%M %p"))

      updateSelectInput(session, "frequency", selected = "Weekly")
      session$sendCustomMessage(type = "resetFileInputHandler", "scriptUpload")

    }

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
          logLevel = input$cronlogLevel
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
            logLevel = input$cronlogLevel
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

    observeEvent(input$clearAddForm, {
      clearForm()
      values$status <- status("Ready to Cron It Up!", "info")
    })

    callModule(logs, "logTab")

    output$cronJobTable <- DT::renderDataTable({
        x <- values$cronjobs[, c("cronjob", "description", "interval", "nextRun", "logLevel")]
        x$cronjob <- ifelse(nchar(x$cronjob) > 23, paste0(substring(x$cronjob, 1, 20), "..."), x$cronjob)
        x$description <- ifelse(nchar(x$description) > 50, paste0(substring(x$description, 1, 47), "..."), x$description)
        colnames(x) <- c("Name", "Description", "Interval", "Next Run", "Log Level")
        x <- x[order(x$Name), ]
        return(x)
      },
      selection = list(mode = 'single', selected = 1), server = FALSE, rownames = FALSE,
      options = list(pageLength = 5, dom="tip"), filter = "top"
    )

    output$status <- renderText(values$status)

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer("crontabR", width = 800, height = 650)


  runGadget(ui, server, viewer = viewer)

}

#crontabAddin()
