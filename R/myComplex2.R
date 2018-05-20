#' myComplex2 object
#'
#' An S4 class to represent complex numbers
#'
#' @import methods
#' @importClassesFrom S4Vectors SimpleList
myComplex2 <- setClass("myComplex2", contains = "SimpleList")

setValidity("myComplex2", function(object) {
  msg <- NULL
  valid <- TRUE
  if (length(object@listData) > 2) {
    valid <- FALSE
    msg <- c(msg, "Complex number must have dimension 2. ")
  }
  if (length(object[[1]]) + length(object[[2]]) < 2) {
    valid <- FALSE
    msg <- c(msg, "Real or imaginary parts cannot be numeric(0). ")
  }
  else if (is.na(object[[1]]) || is.na(object[[2]])) {
    valid <- FALSE
    msg <- c(msg, "Real or imaginary parts cannot be NA")
  }
  if (valid) TRUE else msg
})
