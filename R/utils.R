#' Check Date type
#'
#' @description
#' TRUE is all elements of the vector are Date type, FALSE otherwise.
#'
#' @param vec A vector or string.
#' @returns A Boolean
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @examples
#' non_dates = c('2020-01-01', as.Date('2021-02-02'))
#' is.Date(non_dates)
#' ok_dates = as.Date(c('2020-01-01', '2021-02-02'))
#' is.Date(ok_dates)

is.Date = function(vec) {

    if (!is.vector(vec)) {
        stop("Error: elements must be contained in a vector.")
    }

    inherits(vec, "Date")

}
