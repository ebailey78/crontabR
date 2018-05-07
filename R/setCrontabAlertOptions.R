#'@export
setCrontabAlertOptions <- function(from = NULL, to = NULL, host = NULL, port = NULL,
                                   user = NULL, pass = NULL, ssl = FALSE, tls = FALSE) {

  crontab_alert_options <- list(
    from = from,
    to = to,
    host = host,
    port = port,
    user = user,
    pass = pass,
    ssl = ssl,
    tls = tls
  )

  save(crontab_alert_options, file = paste0(script_directory, "alert_options.rda"))

}
