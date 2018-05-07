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
  library(lubridate)
  if(missing(start_date)) {
    start_date <- as.Date(format(Sys.Date(), "%Y-%m-01")) - months(1)
  }

  if(missing(end_date)) {
    end_date <- Sys.Date() + 1
  }

  files <- paste0(script_directory, unique(format(seq(start_date, end_date, by = 1), "logs/log_%Y%m")))

  log <- do.call(rbind, lapply(files, function(file) {
      if(file.exists(file)) {
        x <- try(read.table(file, header = FALSE, sep = "|", stringsAsFactors = FALSE), silent = TRUE)
        if("try-error" %in% class(x)) {
          x <- readLines(file)
          x <- gsub("\"", "", x)
          y <- strsplit(x, split = "|", fixed = TRUE)
          cn <- y[[1]]
          y <- y[2:length(y)]
          z <- sapply(y, length)
          y <- y[z == 4]
          z <- as.data.frame(do.call(rbind, y), stringsAsFactors = FALSE)
          colnames(z) <- cn
          return(NULL)
        } else {
          return(x)
        }
      }
  }))

  if(!is.null(log)) {

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
