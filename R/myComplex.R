#' myComplex objects
#'
#' An S4 class to represent complex numbers
#'
#' @slot a real part
#' @slot b imaginary part
#'
#' @import methods
#' @export myComplex
#' @exportClass myComplex
myComplex <- setClass("myComplex", slots = c(a = "numeric", b = "numeric"))

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
