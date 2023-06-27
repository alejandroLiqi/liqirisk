#' Years Difference
#'
#' Calculates the times passed between dates, e.g. how many years occured between 2020 and 2021 = 1.
#'
#' @param base_date lower bound date, must be a date clase object.
#' @param tap_date upper bound date, must be a date clase object.
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#'
#'
#' @return a one row data.table keeping original names.
#' @export

diff_years = function(base_date, tap_date) {

    lubridate::interval(base_date, tap_date) / lubridate::years(1)

}
