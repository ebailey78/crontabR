#'Log to the rAutomate Logging System
#'
#'@param msg The message to log
#'@param level The loglevel to log to
#'
#'@export
autolog <- function(msg, app = Sys.getenv("AUTOMATER_APP"), level = "info") {

  levels <- c(error = 0, warn = 1, info = 2, verbose = 3, debug = 4, silly = 5)

  if(level %in% names(levels)) {

    if(!dir.exists(dirname(log_file))) dir.create(dirname(log_file), recursive = TRUE)

    msg <- gsub("|", ".", msg, fixed = TRUE)
    if(app == "") app = "Unknown"
    rec <- list(level, app, Sys.time(), msg)

    write.table(rec, log_file, append = TRUE, sep = "|", row.names = FALSE, col.names = FALSE)

  } else {

    autolog(paste0("Unrecognized logging level: ", level), app, "error")
    warning("Unrecognized logging level: ", level)

  }

}
