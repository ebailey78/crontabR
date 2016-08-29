optionsOutput <- function(id) {
  ns <- NS(id)

  load(paste0(script_directory, "alert_options.rda"))

  to <- try(strsplit(crontab_alert_options$to, "@", fixed = TRUE)[[1]])

  if(length(to) == 2) {

    if(to[2] %in% carriers) {
      crontab_alert_options$type = "text"
      crontab_alert_options$phone_number = to[1]
      crontab_alert_options$carrier = to[2]
    } else {
      crontab_alert_options$type = "email"
      crontab_alert_options$email = crontab_alert_options$to
      crontab_alert_options$phone_number = ""
      crontab_alert_options$carrier = ""
    }

  }

  miniTabPanel("Options", icon = icon("wrench"),
               miniContentPanel(
                 column(width = 6,
                   panel(title = "Alerts",
                     horizontalForm(input_width = 8,
                       textInput(ns("from"), label = "From", value = crontab_alert_options$from),
                       selectInput(ns("type"), "Alert Type", choices = c("text", "email"), selected = crontab_alert_options$type)
                     ),
                     conditionalPanel(paste0("input['", ns("type"), "'] == 'text'"),
                       horizontalForm(input_width = 8,
                         textInput(ns("phone_number"), "Number", placeholder = "(xxx)xxx-xxxx", value = crontab_alert_options$phone_number),
                         selectInput(ns("carrier"), "Carrier", choices = carriers, selected = crontab_alert_options$carrier)
                       )
                     ),
                     conditionalPanel(paste0("input['", ns("type"), "'] == 'email'"),
                       horizontalForm(input_width = 8,
                         textInput(ns("email_address"), "Email", value = crontab_alert_options$email)
                       )
                     )
                    )
                  ),
                 column(width = 6,
                    panel(title = "SMTP Settings",
                     horizontalForm(input_width = 8,
                       textInput(ns("host"), "Host", value = crontab_alert_options$host),
                       textInput(ns("port"), "Port", value = crontab_alert_options$port),
                       textInput(ns("user"), "Username", value = crontab_alert_options$user),
                       passwordInput(ns("pass"), "Password", value = crontab_alert_options$pass)
                     ),
                     column(width = 4, offset = 4, checkboxInput(ns("ssl"), "SSL", value = crontab_alert_options$ssl)),
                     column(width = 4, checkboxInput(ns("tls"), "TLS", value = crontab_alert_options$tls))
                   )
                 ),
                  tags$div(style = "position: absolute; right: 5px; bottom: 5px;",
                    actionButton(ns("test"), "Test Alert", icon = icon("paper-plane-o")),
                    actionButton(ns("update"), "Update Options", icon = icon("refresh"))
                 )

               )
  )
}

optionsServer <- function(input, output, session) {

  observeEvent(input$update, {

    if(input$update > 0) {

      if(input$type == "text") {
        to <- paste0(input$phone_number, "@", input$carrier)
      } else {
        to <- input$email_address
      }

      setCrontabAlertOptions(
        from = input$from,
        to = to,
        host = input$host,
        port = input$port,
        user = input$user,
        pass = input$pass,
        ssl = input$ssl,
        tls = input$tls
      )
    }
  })

  observeEvent(input$test, {
    sendCrontabAlert("Test", "Test crontabR alert")
  })

}

