#' Percentage of Missing Observations
#'
#' Calculates the percentage of missing observations for each column. It can be numeric (0.x) or presentation-ready (x%).
#'
#' @param DT a data.table to obtain column names from.
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#'
#'
#' @return a one row data.table keeping original names.
#' @export

missing_perc = function(DT) {

    DT[DT == ""] = NA

    DT[, lapply(data.table::.SD, function(x) {
                                paste0(round(((sum(is.na(x)) / data.table::.N) * 100), digits = 2), '%')
                                    }) ]

    return(DT)

}
