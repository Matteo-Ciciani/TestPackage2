#' @import methods
myComplex <- setClass("myComplex", slots = c(a = "numeric", b = "numeric"))
c <- myComplex(a = 1.2, b = 3.5)

setValidity("myComplex", function(object) {
  msg <- NULL
  valid <- TRUE
  if (length(object@a) + length(object@b) < 2) {
    valid <- FALSE
    msg <- c(msg, "Real or imaginary parts cannot be numeric(0). ")
  }
  else if (is.na(object@a) || is.na(object@b)) {
    valid <- FALSE
    msg <- c(msg, "Real or imaginary parts cannot be NA")
  }
  if (valid) TRUE else msg
})
