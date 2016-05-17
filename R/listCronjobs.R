#'List Loaded Cronjobs
#'
#'Read your personal crontab and return a list of jobs added by \code{automateR}.
#'
#'@export
listCronjobs <- function() {

  crontab <- readCrontab()

  x <- regexec("##### automateR: (.*) #####", crontab)

  m <- regmatches(crontab, x)

  m <- unlist(sapply(m, function(x) {
    if(length(x) > 0) {
      return(x[2])
    } else {
      return(NULL)
    }
  }))

  m <- unique(m)

  return(m)

}
