checkTimeField <- function(tf) {

  # Regular expression to check time fields
  re <- "((([0-5][0-9])|[0-9]|\\*)\t(([0-9]|1[0-9]|2[0-3])|\\*)\t(([1-9]|[1-2][0-9]|3[0-1])|\\*)\t([1-9]|1[0-2]|\\*)\t([0-7]|\\*))|@(reboot|yearly|annually|monthly|weekly|daily|midnight|hourly)"
  grepl(re, tf)

}
