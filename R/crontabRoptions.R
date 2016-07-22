#'@export
crontabRoptions <- function(...) {

  new_options <- list(...)
  if("try-error" %in% class(suppressWarnings(try(load(file.path(script_directory, "options.rda")), silent = TRUE)))) {
    crontabroptions <- list()
  }

  if(length(new_options) == 0) {
    options(crontabRoptions = crontabroptions)
  } else {
    for(x in seq_along(new_options)) {
      crontabroptions[[names(new_options)[x]]] <- new_options[[x]]
    }
    save(crontabroptions, file = file.path(script_directory, "options.rda"))
    crontabRoptions()
  }

}
