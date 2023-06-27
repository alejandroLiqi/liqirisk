#' Interger 64 Vectors
#' Evaluates if the elements on a vector are of class "interger64". Relevant for data.table functions.
#'
#'
#' is.interger64 is a more general test of an object being interpretable as integers 64 bit encoded
#'
#'
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#' @param x object to be tested.
#'
#' @return a boolean vector type TRUE if interger64, FALSE otherwise.
#' @export
#'
is.integer64 = function(x) {

  class(x) == "integer64"

}
