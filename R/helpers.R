script_directory <- "~/.crontabR/"
alert_options_file <- paste0(script_directory, "alert_options.rda")
log_file <- paste0(script_directory, format(Sys.Date(), "logs/log_%Y%m"))
jobregex <- "##### crontabR: (.*) #####"
dateTimeFormat <- "%Y-%m-%d %I:%M%p"

.onLoad <- function(libname, pkgname) {

  if(!dir.exists(script_directory)) {
    dir.create(script_directory)
  }

  if(!dir.exists(paste0(script_directory, "/logs"))) {
    dir.create(paste0(script_directory, "/logs"))
  }

  if(!file.exists(alert_options_file)) {
    setCrontabAlertOptions()
  }

  if(!file.exists(log_file)) {
    file.create(log_file)
  }

}

makeTag <- function(name) {

  paste0("##### crontabR: ", name, " #####")

}

formatNames <- function(name, verbose = FALSE) {

  name2 <- toupper(name)
  name2 <- gsub("(?!_)[[:punct:]]", "", name2, perl=TRUE)
  name2 <- gsub(" ", "_", name2)

  if(name != name2 & verbose) {
    message("Reformatting name to meet requirements: ", name2)
  }

  return(name2)

}

formatScheduledTime <- function(scheduled_time) {

  if(is.list(scheduled_time)) {

    time_labels <- c("minute", "hour", "day_of_month", "month", "day_of_week")
    mon <- list(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6, Jul = 7,
                Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
    day <- list(Sun = 0, Mon = 1, Tue = 2, Wed = 3, Thu = 4, Fri = 5, Sat = 6)

    if(!is.null(scheduled_time$month)) {
      if(scheduled_time$month %in% names(mon)) {
        scheduled_time$month <- mon[[scheduled_time$month]]
      }
    }

    if(!is.null(scheduled_time$day_of_week)) {
      if(scheduled_time$day_of_week %in% names(day)) {
        scheduled_time$day_of_week <- day[[scheduled_time$day_of_week]]
      }
    }

    for(tl in time_labels) {

      if(!tl %in% names(scheduled_time)) {
        scheduled_time[[tl]] <- "*"
      }

    }

    st <- paste(scheduled_time$min,
                scheduled_time$hour,
                scheduled_time$day_of_month,
                scheduled_time$month,
                scheduled_time$day_of_week,
                sep = "\t")

  } else {

    st <- gsub(" ", "\t", scheduled_time)
    st <- scheduled_time

  }

  if(checkTimeField(st)) {
    return(st)
  } else {
    stop("Invalid time format detected")
  }


}
