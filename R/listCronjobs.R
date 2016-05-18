#'List Loaded Cronjobs
#'
#'Read your personal crontab and return a list of jobs added by \code{automateR}.
#'
#'@export
listCronjobs <- function() {

  crontab <- readCrontab()

  x <- unique(crontab[grep(jobregex, crontab)])

  y <- as.data.frame(do.call(rbind, lapply(x, function(i) {

    y <- grep(i, crontab)
    j <- crontab[y[1]:y[2]]

    name <- regmatches(j[1], regexec(jobregex, j[1]))[[1]][2]
    desc <- paste(collapse = " ", gsub("^#\\|#", "", j[grep("^#\\|#", j)]))
    cronString <- readCronString(strsplit(j[length(j) - 1], "Rscript")[[1]][1])
    interval <- cronString$interval
    nextRun <- format(cronString$nextRun, "%Y-%m-%d %I:%M%p")

    ev <- j[!grepl("^#", j)]
    ev <- ev[-length(ev)]
    ev <- paste(ev, collapse = "\n")

    c(name, desc, interval, nextRun, ev)

  })), stringsAsFactors = FALSE)

  colnames(y) <- c("cronjob", "description", "interval", "nextRun", "envvar")

  return(y)

}
