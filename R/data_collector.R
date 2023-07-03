#' List of available Balance Sheets - Builder - Data Collector
#'
#' Retrieve from a given CERVD_ID (API Call get_balancesheet_list) the Balance Sheets available, type and date.
#'
#' @param num_bs ...
#' @param lista the returned API call
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#' @return a data.table containing the balance sheets info by row
#'
#'
#' @export

lister_bs = function(num_bs, lista) {

    if(length(lista$bilanci) == 1) {

        bilancio =
            data.table(
                num = num_bs,
                tipo_bilancio = lista$bilanci$tipo_bilancio,
                data_chiusura = lista$bilanci$data_chiusura
            )

    } else {

        bilancio =
            data.table(
                num = num_bs,
                tipo_bilancio = lista$bilanci[num_bs][[1]]$tipo_bilancio,
                data_chiusura = lista$bilanci[num_bs][[1]]$data_chiusura
            )

    }

    return(bilancio)

}
