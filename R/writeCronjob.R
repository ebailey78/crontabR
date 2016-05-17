writeCronjob <- function(name, env.vars, scheduled_time) {

  name <- formatNames(name)

  tag <- makeTag(name)

  cronjob <- c(tag)

  if(!missing(env.vars)) {
    if(length(env.vars) > 0) {
      for(i in seq_along(env.vars)) {

        n <- names(env.vars)[i]
        v <- env.vars[i]

        cronjob <- c(cronjob, paste0(n, "=", v))

      }
    }
  }

  cmd <- paste0("Rscript ", script_directory, name, ".R")

  cronjob <- c(cronjob, paste0(scheduled_time, "\t", cmd))

  cronjob <- c(cronjob, tag)

  return(cronjob)

}
