#'@rdname logging
#'
#'@param name The name of the currently active script
#'@param desc The description of the currently active script
#'
#'@details
#'\code{setCronjobValues} and \code{clearCronjobValues} are automatically added
#'to scripts processed by \code{crontabR} so that the loggin system will be able
#'to assign the correct name to the log entries. There shouldn't be any need for
#'a user to use these functions.
#'@export
setCronjobValues <- function(name, desc) {

  v <- list(
    name = name,
    description = desc
  )

  options("crontabRjobValues" = v)

}

#'@export
clearCronjobValues <- function() {

  options("crontabRjobValues" = NULL)

}
