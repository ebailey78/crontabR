#'Logging
#'
#'Log to the rAutomate Logging System
#'
#'@param msg The message to log
#'@param level The loglevel to log to
#'@param app The app to which to associate the log entry
#'
#'@details
#'\code{crontabR} contains a logging system that will automatically log most errors
#'and warnings to a log file saved in \code{~/.crontabR/logs/log}. In addition, you
#'may use the \code{cronLog} function to log additional information to the log. The
#'log can be one of six different levels; error, warn, info, verbose, debug, or silly.
#'
#'@rdname logging
#'@export
cronLog <- function(msg, level = "info", app = getOption("crontabRjobValues")$name) {

  if(is.null(app)) app = "crontabR"

  ll <- getOption("crontabRjobValues")$logLevel
  tl <- getOption("crontabRjobValues")$textLevel

  if(is.null(ll)) ll <- "info"
  if(is.null(tl)) tl <- "none"

  levels <- c(none = -1, error = 0, warn = 1, info = 2, verbose = 3, debug = 4, silly = 5)

  if(level %in% names(levels)) {

    if(levels[level] <= levels[ll]) {

      if(!dir.exists(dirname(log_file))) dir.create(dirname(log_file), recursive = TRUE)

      msg <- gsub("|", ".", msg, fixed = TRUE)
      rec <- list(level, app, Sys.time(), msg)

      write.table(rec, log_file, append = TRUE, sep = "|", row.names = FALSE, col.names = FALSE)

    }

    if(levels[level] <= levels[tl]) {

      sendCrontabAlert("crontabR Alert", paste0(app, " - ", level, ": ", msg))

    }



  } else {

    cronLog(paste0("Unrecognized logging level: ", level), "error", app)
    warning("Unrecognized logging level: ", level)

  }

}

#'@rdname logging
#'
#'@param expr an expression to be evaluated with crontabR logging.
#'
#'@details
#'\code{logErrors} isn't meant to be used by users. Each script that is automated
#'with \code{crontabR} is wrapped in this function so that errors, warnings, and
#'messages are automatically logged. Errors are logged at level \code{error},
#'warnings are logged at level \code{warn}, and messages are logged at level
#'\code{verbose}. In addition, any value the script returns will be logged at
#'level \code{info}.
#'
#'@export
logErrors <- function(expr) {

  x <- withCallingHandlers({expr},
                             warning = function(w) {
                               cronLog(w$message, "warn")
                             },
                             error = function(e) {
                               cronLog(e$message, "error")
                             },
                             message = function(m) {
                               cronLog(m$message, "verbose")
                             }
  )

  if(length(x) > 0) {
    x <- try(as.character(x))
    if(!"try-error" %in% class(x)) cronLog(as.character(x), "info")
  }

  return(x)

}
