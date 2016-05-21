#'List Loaded Cronjobs
#'
#'Read your personal crontab and return a list of jobs added by \code{automateR}.
#'
#'@export
listCronjobs <- function() {

  crontab <- readCrontab()

  x <- unique(crontab[grep(jobregex, crontab)])

  if(length(x) > 0) {

    y <- as.data.frame(do.call(rbind, lapply(x, function(i) {

      y <- grep(i, crontab)
      j <- crontab[y[1]:y[2]]

      name <- regmatches(j[1], regexec(jobregex, j[1]))[[1]][2]
      desc <- paste(collapse = " ", gsub("^#\\|#", "", j[grep("^#\\|#", j)]))
      cronString <- readCronString(strsplit(j[length(j) - 1], "source|Rscript")[[1]][1])
      interval <- cronString$interval
      nextRun <- format(cronString$nextRun, "%Y-%m-%d %I:%M%p")
      bashrc <- grepl("source $HOME/.bashrc", j[length(j) - 1])

      ev <- j[!grepl("^#", j)]
      ev <- ev[-length(ev)]
      ev <- paste(ev, collapse = "\n")

      c(name, desc, interval, nextRun, ev, bashrc)

    })), stringsAsFactors = FALSE)

    colnames(y) <- c("cronjob", "description", "interval", "nextRun", "envvar", "bashrc")

  } else {
    y <- data.frame(cronjob = character(), description = character(), interval = character(), nextRun = character(), envvar = character(), bashrc = logical(), stringsAsFactors = FALSE)
  }

  return(y)

}
