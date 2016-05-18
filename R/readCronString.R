#'@export
readCronString <- function(cronString) {

  cronList <- strsplit(cronString, "\t| ")[[1]]
  cronList <- cronList[cronList != ""]
  if(length(cronList) != 5) {
    stop("Inproperly formatted cron string.")
  }
  cronList <- suppressWarnings(as.integer(cronList))
  cronList <- as.list(cronList)
  names(cronList) <- c("M", "H", "d", "m", "W")

  return(list(
    interval = getInterval(cronList),
    nextRun = getNextRun(cronList)
  ))


}

getInterval <- function(cronList) {

  v <- paste(sapply(cronList, function(x) ifelse(is.na(x), "x", "o")), collapse = "")

  intervals <- list(
    "oxxxx" = "Hourly",
    "ooxxx" = "Daily",
    "ooxxo" = "Weekly",
    "oooxx" = "Monthly",
    "oooox" = "Yearly"
  )

  return(intervals[[v]])

}

timeList <- function(x) {
  x <- as.POSIXlt(x)
  list(
    M = as.integer(format(x, "%M")),
    H = as.integer(format(x, "%H")),
    d = as.integer(format(x, "%d")),
    m = as.integer(format(x, "%m")),
    W = as.integer(x$wday)
  )
}

getNextRun <- function(cronList) {

  interval <- getInterval(cronList)
  nr <- Sys.time()
  second(nr) <- 0
  now <- timeList(nr)
  print(interval)
  print(cronList)
  switch(interval,
    Hourly = {
      minute(nr) <- cronList$M
      if(now$M > cronList$M) {
        hour(nr) <- hour(nr) + 1
      }
    },
    Daily = {
      minute(nr) <- cronList$M
      hour(nr) <- cronList$H
      if(now$H > cronList$H) {
        day(nr) <- day(nr) + 1
      }
    },
    Weekly = {
      minute(nr) <- cronList$M
      hour(nr) <- cronList$H
      wday(nr) <- cronList$W + 1
      if(nr < Sys.time()) {
        nr <- nr + weeks(1)
      }
    },
    Monthly = {
      minute(nr) <- cronList$M
      hour(nr) <- cronList$H
      day(nr) <- cronList$d
      if(now$d > cronList$d) {
        month(nr) <- month(nr) + 1
      }
    },
    Yearly = {
      minute(nr) <- cronList$M
      hour(nr) <- cronList$H
      day(nr) <- cronList$d
      month(nr) <- cronList$m
      if(nr < Sys.time()) {
        year(nr) <- year(nr) + 1
      }
    }
  )

  return(nr)

}

# cronString <- "46\t*\t*\t*\t*"    # Hourly
# cronString <- "46\t04\t*\t*\t*"   # Daily
# cronString <- "46\t04\t*\t*\t2"   # Weekly
# cronString <- "46\t04\t11\t*\t*"  # Monthly
# cronString <- "46\t04\t05\t05\t*" # Yearly
# cronString <- "46\t\t04 \t 05  05   *   " # Yearly
