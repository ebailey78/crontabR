#'Task Scheduling
#'
#'Add/remove a cronjob to automate an R script to your personal crontab
#'
#'@rdname task_scheduling
#'
#'@param name A unique name for this cronjob
#'@param desc A description of what the script does
#'@param env_vars A list of environment variables that need to be set for this cronjob
#'@param scheduled_time A list or string describing when the task should be run.
#'@param script_path The filepath to the script you want to automate
#'@param overwrite If the cronjob already exists, should it be overwritten?
#'@param verbose Should extra messages be displayed while the cronjob is being built?
#'@param warn Should errors stop the function or just give an warning?
#'@param bashrc Should the user's .bashrc file be loaded before running the script?
#'
#'@details
#'\code{cron} does not start a shell and therefore doesn't load environment variables that would normally get loaded. Use
#'\code{env.var} to set variables that are needed by your script and/or set \code{bashrc = TRUE} to load the user's
#'.bashrc file each time before the script is run.
#'
#'\code{scheduled_time} should be either a named list containing any combination of \code{minute} (0-59), \code{hour} (0-23),
#'\code{day_of_month}(1-31), \code{month}(1-12)(Jan-Dec), \code{day_of_week}(0-7)(Sun-Sat) or a properly formatted cron string
#'For example, \code{scheduled_time = list(minute=30, hour=7, day_of_week=2)} would result in the task being scheduled for
#'7:30AM every Tuesday. \code{scheduled_time = "30 7 * * 2"} would accomplish the same thing.
#'
#'\code{script_path} should point to the location of a script you wish to automate. The script will be copied to
#'the \code{.crontabR} directory in your home directory. Additional code will be copied to the beginning and end
#'of the script so that \code{crontabR}'s logging functions will work. It is this modified copy of the script
#'that will be run by \code{crontabR}.
#'
#'@export
addCronjob <- function(name, desc = NULL, env_vars, scheduled_time, script_path,
                       overwrite = FALSE, verbose = FALSE, warn = FALSE, bashrc = TRUE) {

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

    if(processScript(name, desc, script_path, overwrite, warn = warn)) {

      cronjob <- writeCronjob(name, desc, env_vars, scheduled_time, bashrc)
      crontab <- readCrontab()
      crontab <- c(crontab, cronjob)
      writeCrontab(crontab)
      if(verbose) message("Cronjob: ", name, " added to crontab.")
      return(TRUE)

    }
  }

}
