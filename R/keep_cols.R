#' Keep Columns
#'
#' A function that emulates dplyr's .contains helper function combined with select, to subset columns combining a list and a pattern.
#'
#' @param DT a data.table to obtain column names from.
#' @param pattern a string containing the patterns to look for in the column names
#' @param ... other arguments coming from the grep function
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#' @return a vector to be plugged into a data.table's "..j" for subsetting.
#'
#'
#' @export

# keep columns with pattern and other

keep_cols = function(DT, pattern, ...) {

    keeped = unlist(c(list(...), grep(as.character(pattern), names(DT), value = TRUE)), recursive = TRUE, use.names = TRUE)

    return(keeped)

}
