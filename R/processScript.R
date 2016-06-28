processScript <- function(name, desc, script_path, logLevel = "info", overwrite = FALSE, warn = FALSE, verbose = FALSE) {

  if(!file.exists(script_path)) {
    stop("Could not locate script: ", script_path)
  }

  new_script_path <- paste0(script_directory, name, ".R")

  if(file.exists(new_script_path) & !overwrite) {
    err <- paste0("File ", basename(script_path), " has already been processed for crontabR. Please set `overwrite = TRUE` to overwrite the existing version.")
    if(warn) {
      warning(err)
      return(FALSE)
    } else {
      stop(err)
    }
  } else {

    if(!dir.exists(script_directory)) {
      dir.create(script_directory)
    }

    script <- readLines(script_path)

    header <- c(
      paste("## crontabR Automation Script for", name),
      "",
      paste("##", desc),
      "",
      "local({",
      "library(crontabR)",
      paste0("setCronjobValues('", name, "', '", desc, "', '", logLevel, "')"),
      "cronLog(\"Script Started\")",
      "",
      "logErrors({",
      "##### Do not edit above this line #####",
      ""
    )

    footer <- c(
      "",
      "##### Do not edit below this line #####",
      "})",
      "",
      "cronLog(\"Script Complete\")",
      "clearCronjobValues()",
      "})"
    )

    script <- c(header, script, footer)

    writeLines(script, new_script_path)

    return(TRUE)

  }

}
