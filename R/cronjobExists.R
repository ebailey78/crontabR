#'Check if A Cronjob Exists
#'
#'Read your personal crontab and see if a crontab of the given name has been added by \code{automateR}.
#'
#'@param name The unique name of the cronjob you want to test
#'
#'@export
cronjobExists <- function(name) {

  name <- formatNames(name)

  sum(grepl(makeTag(name), readCrontab())) > 0

}

