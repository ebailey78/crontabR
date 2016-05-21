#'@export
setCronjobValues <- function(name, desc) {

  v <- list(
    name = name,
    description = desc
  )

  options("crontabRjobValues" = v)

}
