#' Check Date type
#'
#' @description
#' TRUE is all elements of the vector are Date type, FALSE otherwise.
#'
#' @param vec A vector or string.
#' @returns A Boolean
#' @author Alejandro Abraham <alejandro@liqi.it>

is.Date = function(vec) {

    if (!is.vector(vec)) {
        stop("Error: elements must be contained in a vector.")
    }

    inherits(vec, "Date")

}
