#'@export
setCrontabAlertOptions <- function(from, to, host, port, user, pass,
                                   ssl = FALSE, tls = FALSE) {

  print(to)

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
