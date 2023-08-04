#' Invoices forecast - Model preparation
#'
#' Prepare data.table for model fitting
#'
#' @param DT is a data.table containing invoices from "ricevute.csv" or "emesse.csv" probably
#' @param c_ds is column from DT to be indicated as date in the prophet model
#' @param c_y is column from DT to be indicated as y (target) in the prophet model
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#' @return data.table with DT ready for model fitting with prophet
#'
#'
#' @export






#' Invoices forecast - Model forecast (Prophet)
#'
#' Fit Liqi proprietory Prophet Model application
#'
#' @param DT is a data.table created with model_prep()
#' @param col_forecast is column from DT to be indicated as target
#' @param n_weeks number of weeks ahead to forecast
#' @param complete_dt boolean to return full prophet data.table output or same DT from model_prep() with n_weeks forecast
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#' @import prophet
#' @return data.table with DT ready for model fitting with prophet
#'
#'
#' @export



