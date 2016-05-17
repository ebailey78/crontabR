#'@export
getLog <- function() {

  if(file.exists(log_file)) {

    log <- read.table(log_file, header = FALSE, sep = "|", stringsAsFactors = FALSE)
    colnames(log) <- c("level", "job", "date", "message")
    log$date <- as.POSIXlt(log$date)

  } else {

    log <- data.frame(level = "", job = "", date = "", message = "")

  }

  return(log)

}
