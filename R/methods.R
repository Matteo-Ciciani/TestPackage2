#' @include myComplex.R myComplex2.R generics.R
NULL

#' Methods for \code{\link{myComplex}} class
#'
#' @name myComplex-methods
NULL

#' @rdname myComplex-methods
setMethod("show", signature = "myComplex", definition = function(object) {
  cat("(", object@a, ", ", object@b, ")", sep = "")
  invisible(NULL)
})

setMethod("real", signature = "myComplex", definition = function(object) object@a)

setMethod("+", signature = "myComplex", definition = function(e1, e2) {
  .a <- e1@a + e2@a
  .b <- e1@b + e2@b
  myComplex(a = .a, b = .b)
})

setMethod("show", signature = "myComplex2", definition = function(object) {
  cat("(", object[[1]], ", ", object[[2]], ")", sep = "")
  invisible(NULL)
})

setMethod("real", signature = "myComplex2", definition = function(object) object[[1]])
