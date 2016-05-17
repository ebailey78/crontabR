#'Automate an R script
#'
#'Add a cronjob to automate an R script to your personal crontab
#'
#'@param name A unique name for this cronjob
#'@param env_vars A list of environment variables that need to be set for this cronjob
#'@param scheduled_time A list or string describing when the task should be run.
#'@param script_path The filepath to the script you want to automate
#'@param overwrite If the cronjob already exists, should it be overwritten?
#'@param verbose Should extra messages be displayed while the cronjob is being built?
#'
#'@details
#'\code{cron} does not start a shell and therefore doesn't load environment variables that would normally get loaded. Use
#'\code{env.var} to set variables that are needed by your script.
#'
#'\code{scheduled_time} should be either a named list containing any combination of \code{minute} (0-59), \code{hour} (0-23),
#'\code{day_of_month}(1-31), \code{month}(1-12)(Jan-Dec), \code{day_of_week}(0-7)(Sun-Sat). For example, \code{scheduled_time =
#'list(minute=30, hour=7, day_of_week=2)} would result in the task being scheduled for 7:30AM every Tuesday.
#'
#'Alternately, you can enter a formatted crontab time field, with each value seperated by a single space. For example,
#'\code{scheduled_time="30 7 * * 2"} would also schedule the task to run at 7:30AM every Tuesday.
#'
#'\code{script_path} should point to the location of a script you wish to automate. The script will be copied to
#'the \code{.automateR} directory in your home directory. It is this copy of the script that will be run by \code{automateR}.
#'
#'@export
addCronjob <- function(name, env_vars, scheduled_time, script_path,
                       overwrite = FALSE, verbose = FALSE, warn = FALSE) {

  name <- formatNames(name, verbose)
  scheduled_time <- formatScheduledTime(scheduled_time)

  if(cronjobExists(name) & !overwrite) {
    err <- paste0("Cronjob already exists. Set `overwrite = TRUE` to overwrite the existing cronjob or give this cronjob a different name.")
    if(warn) {
      warning(err)
      return(FALSE)
    } else {
      stop(err)
    }
  } else {

    if(cronjobExists(name)) deleteCronjob(name)

    if(processScript(name, script_path, overwrite, warn = warn)) {

      cronjob <- writeCronjob(name, env_vars, scheduled_time)
      crontab <- readCrontab()
      crontab <- c(crontab, cronjob)
      writeCrontab(crontab)
      if(verbose) message("Cronjob: ", name, " added to crontab.")
      return(TRUE)

    }
  }

}