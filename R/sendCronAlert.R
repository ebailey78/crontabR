#'@export
sendCrontabAlert <- function(subject, message) {

  if(require("mailR")) {

    if(file.exists(alert_options_file)) {

      load(alert_options_file)

      mailR::send.mail(from = crontab_alert_options$from,
                to = crontab_alert_options$to,
                subject = subject,
                body = message,
                html = FALSE,
                smtp = list(host.name = crontab_alert_options$host,
                            port = crontab_alert_options$port,
                            user.name = crontab_alert_options$user,
                            passwd = crontab_alert_options$pass,
                            ssl = crontab_alert_options$ssl),
                authenticate = crontab_alert_options$user != "",
                send = TRUE)
    } else {
      warning("No crontab_alerts_options found. Please run the setCrontabAlertOptions() function. Message not sent.")
    }
  } else {
    warning("The mailR package was not found. Please install mailR. Message not sent.")
  }

}
