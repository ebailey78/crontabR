#'@rdname task_scheduling
#'
#'@details
#'\code{cronjobExists} reads your personal crontab to see if a cronjob of the
#'given name has been added by \code{crontabR}.
#'
#'@export
cronjobExists <- function(name) {

  name <- formatNames(name)

  sum(grepl(makeTag(name), readCrontab())) > 0

}

