#'@rdname task_scheduling
#'
#'@details
#'\code{deleteCronjob} will remove the lines in your crontab associated with the
#'named cronjob. It will also delete the script from the \code{.crontabR} directory.
#'
#'@export
deleteCronjob <- function(name) {

  name <- formatNames(name)

  if(cronjobExists(name)) {

    tag <- makeTag(name)
    crontab <- readCrontab()
    cronjob_tags <- grep(tag, crontab)

    cronjob_lines <- seq(cronjob_tags[1], cronjob_tags[2])

    crontab <- crontab[-cronjob_lines]

    writeCrontab(crontab)

    file.remove(paste0(script_directory, name, ".R"))

  } else {
    stop(name, " not found in crontab.")
  }


}
