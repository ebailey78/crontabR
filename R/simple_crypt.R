simple_crypt <- function(txt, key, encrypt = TRUE) {

  create_crypt_key <- function(key, encrypt = TRUE) {

    set.seed(3132009)
    x <- c(letters, LETTERS, 0:9, "!", "@", "#", "$", "%", "^", "&", "*", "(",
           ")", "{", "}", "[", "]", "?", ".", ",", ":", ";", "\"", "\\", "/",
           "<", ">", " ", "~", "`")
    x <- sample(x, length(x))

    key <- strsplit(key, "")[[1]]
    keyval <- seq_along(x)
    names(keyval) <- x
    root <- 1/sqrt(length(key))
    key <- as.integer(prod(keyval[key])^root) + 1

    set.seed(key)

    y <- sample(x, length(x))
    if(encrypt) {
      names(y) <- x
      return(y)
    } else {
      names(x) <- y
      return(x)
    }

  }

  y <- create_crypt_key(key, encrypt)

  z <- strsplit(txt, "")[[1]]
  enc <- y[z]
  enc[is.na(enc)] <- z[is.na(enc)]
  return(paste(enc, collapse = ""))

}
