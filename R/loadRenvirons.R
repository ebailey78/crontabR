#'@export
loadRenvirons <- function() {

  re <- c(
    paste0(Sys.getenv("R_HOME"), "/etc/Renviron.site"),
    paste0(Sys.getenv("HOME"), "/.Renviron")
  )

  for(x in re) {if(file.exists(x))  {
    readRenviron(x)
    message("loaded ", x)
  }}

}
