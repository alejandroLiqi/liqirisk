#' Excel's LEFT
#'
#' A function that emulates Excel's string functions LEFT(), MID() and RIGHT().
#'
#' @param string string or vector containig strings to parse.
#' @param n number of characters to extract.
#' @param i character number from which to start counting.
#'
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#' @return a vector with the remaining characters from the original strings
#' @export
#'
str_left = function(string, i = 1, n) {

  substr(string, i, n)

}

