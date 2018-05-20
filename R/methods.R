#' @include myComplex.R myComplex2.R generics.R
NULL

#' Methods for \code{\link{myComplex}} class
#'
#' @param object An object of class myComplex
#' @param e1 An object of class myComplex
#' @param e2 An object of class \code{ANY}
#'
#' @name myComplex-methods
NUL

#' Methods for \code{\link{myComplex2}} class
#'
#' @param object An object of class myComplex2
#'
#' @name myComplex2-methods
NULL

#' @rdname myComplex-methods
setMethod("show", signature = "myComplex", definition = function(object) {
  cat("(", object@a, ", ", object@b, ")", sep = "")
  invisible(NULL)
})

#' @rdname myComplex-methods
setMethod("real", signature = "myComplex", definition = function(object) object@a)

#' @rdname myComplex-methods
setMethod("+", signature = "myComplex", definition = function(e1, e2) {
  .a <- e1@a + e2@a
  .b <- e1@b + e2@b
  myComplex(a = .a, b = .b)
})

#' @rdname myComplex2-methods
setMethod("show", signature = "myComplex2", definition = function(object) {
  cat("(", object[[1]], ", ", object[[2]], ")", sep = "")
  invisible(NULL)
})

#' @rdname myComplex2-methods
setMethod("real", signature = "myComplex2", definition = function(object) object[[1]])
