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

model_prep = function(DT, c_ds = 'ds', c_y = 'y') {

    # CHECKS
    if (! data.table::is.data.table(DT)) {
        stop("Error: data must be a data.table.")
    }

    required_cols = c('date', 'amount_net', 'tipo_docum', 'entity_name')
    missing_cols =  setdiff(required_cols, colnames(DT))
    if (length(missing_cols) > 0) { stop("The DT data.table is missing the following required column(s): ", paste(missing_cols, collapse = ", "))}

    all_weeks =  data.table::data.table(ds = seq(min(DT$date), max(DT$date), by = "week"))

    # FUNCTION

    ## Clean DT

    ### Week start
    kc_DT = c('date', 'amount_net', 'tipo_docum', 'entity_name')
    DT = DT[, ..kc_DT]
    DT = DT[, amount_net :=  fifelse(tipo_docum == 'Nota di credito', -amount_net, amount_net)]
    DT = DT[, invoices_cat :=  fifelse(grepl("Fattur", tipo_docum, ignore.case = TRUE), 'sales', 'other')]
    DT[, ds := as.Date(format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d"))]
    DT_agg = DT[, .(y = sum(amount_net)), by = list(ds, invoices_cat)]
    setorder(DT_agg, 'ds')
    DT_agg = all_weeks[DT_agg[invoices_cat == 'sales'], on = "ds"]

    ### FOR NOTA DI CREDITO WE NEED TO CREATE A FUNCTION THAT LOOKS FOR THE SAME AMOUNT OR 3 MONTHS BEFORE AND FINDS THE SIMILAR AMOUNT OR STH

    DT_agg[, ds := as.Date(ds)]

    DT_agg = DT_agg[, .(ds, y)]


    DT_agg[is.na(y), y := 0]

    ## Reordering
    setcolorder(DT_agg, neworder = c('ds', 'y'))
    setkey(DT_agg, 'ds')
    setorder(DT_agg, 'ds')

    setnames(DT_agg, old = names(DT_agg), new = c(c_ds, c_y))


    return(DT_agg)

}




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
#' @import plotly
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @return data.table with DT ready for model fitting with prophet
#'
#'
#' @export

model_forecast = function(DT, col_forecast, n_weeks = 26, complete_dt = FALSE) {

    # CHECKS

    if (! data.table::is.data.table(DT)) {
        stop("Error: data must be a data.table.")
    }

    required_cols = c('ds')
    missing_cols =  setdiff(required_cols, colnames(DT))
    if (length(missing_cols) > 0) { stop("The DT data.table is missing the following required column(s): ", paste(missing_cols, collapse = ", "))}

    if (!is.Date(DT$ds)) {
        stop("Error: ds column must be a date type column.")
    }

    DT = DT[, .(ds, y = get(col_forecast))]
    if (!is.numeric(DT$y)) {
        stop(paste("Error:", col_forecast, 'must be a numeric vector.'))
    }


    # FUNCTION

    ### Outlier detection
    median_value = median(DT$y)
    iqr_value = IQR(DT$y)

    ### Define the range for non-outlier values (e.g., within 3 standard deviations)
    lower_limit = median_value - 3.5 * iqr_value
    upper_limit = median_value + 3.5 * iqr_value

    DT[,  y :=  data.table::fifelse(y < lower_limit | y > upper_limit, median_value, y)]

    model_inv = prophet::prophet(DT, growth = "linear", seasonality.mode = 'multiplicative', daily.seasonality = FALSE, weekly.seasonality = TRUE)

    future_dates = prophet::make_future_dataframe(model_inv, periods = n_weeks, freq = 'week')

    ### Make predictions
    inv_forecast = predict(model_inv, future_dates)
    data.table::setDT(inv_forecast)
    inv_forecast[, type := fifelse(as.Date(ds) <= max(DT$ds), 'Sample', 'Forecast')]

    if(complete_dt == TRUE) {

        return(inv_forecast)

    } else {

        return(inv_forecast[, .(ds, yhat, yhat_lower, yhat_upper, type)])
    }

}


#' Invoices forecast - Model forecast Plot
#'
#' Plot Forecast from Liqi propritory Prophet model
#'
#' @param DT is a data.table created with model_prep()
#' @param type_plot 1 for "Invoices Sent" and 2 for "Invoices Recieved", otherwise a valid HEX Value
#' @param title_plot plot's title
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorder
#' @importFrom data.table setkey
#' @importFrom data.table setcolorder
#' @importFrom data.table setnames
#' @importFrom data.table :=
#' @importFrom data.table fifelse
#' @importFrom prophet prophet
#' @importFrom prophet make_future_dataframe
#' @importFrom("plotly", "layout", "add_trace", "add_ribbons", "plot_ly")
#' @importFrom("stats", "IQR", "median", "predict")
#' @return data.table with DT ready for model fitting with prophet
#'
#'
#' @export

forecast_plot = function(DT, type_plot = '1', title_plot = 'Weekly Forecast') {

    # CHECKS

    if (!data.table::is.data.table(DT)) {
        stop("Error: data must be a data.table.")
    }

    if (!is.character(type_plot)) {
        stop("Error: type_plot must be a string.")
    }

    if (nchar(type_plot) == 1) {
        if (type_plot != '1') {
            if (type_plot != '2') stop("Error: type_plot IN, OUT or a valid HEX Value of the form #YYYYYY.")
        }
    }

    if (nchar(type_plot) != 1) {
        if (nchar(type_plot) != 7) stop("Error: type_plot IN, OUT or a valid HEX Value of the form #YYYYYY.")
    }

    required_cols = c('ds', 'yhat', 'yhat_lower', 'yhat_upper', 'type')
    missing_cols = setdiff(required_cols, colnames(DT))
    if (length(missing_cols) > 0) { stop("The DT data.table is missing the following required column(s): ", paste(missing_cols, collapse = ", "))}

    if(type_plot == '1') {
        color_id = '#3F7C85'
    } else if(type_plot == '2') {
        color_id = '#FF5F5D'
    } else {
        color_id = type_plot
    }


    # FUNCTION

    DT |>
        plotly::plot_ly(type = 'scatter', mode = 'lines') |>
        plotly::add_trace(x = ~DT[type == 'Sample']$ds,
                  y = ~DT[type == 'Sample']$yhat,
                  name = 'Sample',
                  color = I(color_id)) |>
        plotly::add_ribbons(x = ~DT[type == 'Forecast']$ds,
                    ymin = ~DT[type == 'Forecast']$yhat_lower,
                    ymax = ~DT[type == 'Forecast']$yhat_upper,
                    color = I("gray95"),
                    name = "95% confidence") |>
        plotly::add_trace(x = ~DT[type == 'Forecast']$ds,
                  y = ~DT[type == 'Forecast']$yhat,
                  name = 'Forecast',
                  color = I('#94A69F'),
                  line = list(dash = "dash")) |>
        plotly::layout(title = title_plot,
               xaxis = list(title = ''),
               yaxis = list(title = 'yhat'))

}
