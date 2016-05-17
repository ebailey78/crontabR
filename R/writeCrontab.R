writeCrontab <- function(crontab) {

  tf <- tempfile()

  fc <- file(tf)

  writeLines(crontab, fc)

  system(paste("crontab", tf))
  system(paste("rm", tf))

  close(fc)

}
