myComplex <- setClass("myComplex", slots = c(a = "numeric", b = "numeric"))
c <- myComplex(a = 1.2, b = 3.5)

setMethod("show", signature = "myComplex", definition = function(object) {
  cat("(", object@a, ", ", object@b, ")", sep = "")
  invisible(NULL)
})

setGeneric("real", function(object) standardGeneric("real"))

setMethod("real", signature = "myComplex", definition = function(object) object@a)

setMethod("+", signature = "myComplex", definition = function(e1, e2) {
  .a <- e1@a + e2@a
  .b <- e1@b + e2@b
  myComplex(a = .a, b = .b)
})

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

myComplex2 <- setClass("myComplex2", contains = "SimpleList")

setMethod("show", signature = "myComplex2", definition = function(object) {
  cat("(", object[[1]], ", ", object[[2]], ")", sep = "")
  invisible(NULL)
})

setMethod("real", signature = "myComplex2", definition = function(object) object[[1]])

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
