readCrontab <- function() {

  suppressWarnings(system("crontab -l", intern = TRUE))

}
