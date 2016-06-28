#'List Loaded Cronjobs
#'
#'Read your personal crontab and return a data.frame of information about jobs
#'added by \code{crontabR}.
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
      desc <- j[grep("^#\\|#", j)]
      desc <- desc[!grepl("^#\\|#logLevel:", desc)]
      desc <- paste(collapse = " ", gsub("^#\\|#", "", desc))
      ll <- j[grep("^#\\|#logLevel", j)]
      logLevel <- try(regmatches(ll, regexec("^#\\|#logLevel: (.+)", ll))[[1]][2], silent = TRUE)
      if("try-error" %in% class(logLevel)) logLevel <- "info"
      cronString <- readCronString(strsplit(j[length(j) - 1], "source|Rscript")[[1]][1])
      interval <- cronString$interval
      nextRun <- format(cronString$nextRun, "%Y-%m-%d %I:%M%p")
      bashrc <- grepl("source $HOME/.bashrc", j[length(j) - 1], fixed = TRUE)

      ev <- j[!grepl("^#", j)]
      ev <- ev[-length(ev)]
      ev <- paste(ev, collapse = "\n")

      c(name, desc, interval, nextRun, ev, bashrc, logLevel)

    })), stringsAsFactors = FALSE)

    colnames(y) <- c("cronjob", "description", "interval", "nextRun", "envvar", "bashrc", "logLevel")

    y <- y[order(y$cronjob), ]

  } else {
    y <- data.frame(cronjob = character(), description = character(), interval = character(), nextRun = character(), envvar = character(), bashrc = logical(), logLevel = character(), stringsAsFactors = FALSE)
  }

  return(y)

}
