#'Log to the rAutomate Logging System
#'
#'@param msg The message to log
#'@param level The loglevel to log to
#'
#'@export
cronLog <- function(msg, level = "info", app = getOption("crontabRjobValues")$name) {

  if(is.null(app)) app = "crontabR"

  levels <- c(error = 0, warn = 1, info = 2, verbose = 3, debug = 4, silly = 5)

  if(level %in% names(levels)) {

    if(!dir.exists(dirname(log_file))) dir.create(dirname(log_file), recursive = TRUE)

    msg <- gsub("|", ".", msg, fixed = TRUE)
    rec <- list(level, app, Sys.time(), msg)

    write.table(rec, log_file, append = TRUE, sep = "|", row.names = FALSE, col.names = FALSE)

  } else {

    cronLog(paste0("Unrecognized logging level: ", level), "error", app)
    warning("Unrecognized logging level: ", level)

  }

}

#'@export
logErrors <- function(expr) {

  x <- withCallingHandlers({expr},
                             warning = function(w) {cronLog(w$message, "warn")},
                             error = function(e) {cronLog(e$message, "error")},
                             message = function(m) {cronLog(m$message, "verbose")}
  )

  if(length(x) > 0) {
    cronLog(as.character(x), "info")
  }

  return(x)

}
