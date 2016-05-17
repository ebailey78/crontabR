processScript <- function(name, script_path, overwrite = FALSE, warn = FALSE, verbose = FALSE) {

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

    if(!file.copy(script_path, new_script_path, overwrite = TRUE) & verbose) {
      err <- paste0("Unable to process and copy ", script_path)
      if(warn) {
        warning(err)
        return(FALSE)
      } else {
        stop(err)
      }
    } else {
      if(verbose) {
        message(script_path, " processed and added to crontabR.")
      }
    }

    return(TRUE)

  }

}
