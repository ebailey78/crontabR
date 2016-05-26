#'@rdname logging
#'
#'@param levels A list of logging levels to return from the log
#'@param jobs A list of cronjob names to return from the log
#'@param start_date The earliest date for which to return log entries
#'@param end_date The latest date for which to return log entries
#'
#'@details
#'\code{getLog} reads and optionally filters the \code{crontabR} log.
#'@export
getLog <- function(levels, jobs, start_date, end_date) {

  if(file.exists(log_file)) {

    log <- read.table(log_file, header = FALSE, sep = "|", stringsAsFactors = FALSE)
    colnames(log) <- c("level", "job", "date", "message")
    log$date <- format(as.POSIXlt(log$date), dateTimeFormat)

    if(!missing(levels)) {
      log <- log[log$level %in% levels, ]
    }

    if(!missing(jobs)) {
      jobs <- formatNames(jobs)
      log <- log[log$job %in% jobs, ]
    }

    if(!missing(start_date)) {
      log <- log[log$date >= as.POSIXlt(start_date), ]
    }

    if(!missing(end_date)) {
      log <- log[log$date <= as.POSIXlt(end_date), ]
    }

  } else {

    log <- data.frame(level = "", job = "", date = "", message = "")

  }

  return(log)

}
