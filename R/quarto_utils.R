#' Check Files - Quarto Data Collector
#'
#' Verify that all files in the folder (coming from our server) are not NULL
#'
#' @param lista a list() of file names in a folder using list.files()
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @importFrom data.table data.table
#' @return data.table with the number of rows and names of each file in the list
#'
#'
#' @export

check_files = function(lista) {

    dts = data.table::data.table(
        file_name = NA_character_,
        status = NA_character_,
        number_rows = NA_character_)

    for (file in 1:length(lista)) {

        name = names(lista[file])
        n_rows = nrow(data.table::as.data.table(lista[file]))
        status_check = n_rows >= 1
        new_file = data.table::data.table(
            file_name = name,
            status = status_check,
            number_rows = n_rows)
        dts = rbind(dts, new_file)

    }

    dts = dts[-1]
    return(dts)

}



#' Call Out Note - Quarto Data Collector
#'
#' A special type of call-out for Quarto that retrieves "OK" or "WARNING" depending on the condition and messages
#'
#' @param statement a function call to evalaute
#' @param condition the expected value to return TRUE
#' @param message_ok string to display if TRUE
#' @param message_fail string to display if FALSE
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @return HTML code to be inserted in Quarto with eval='asis'
#'
#'
#' @export
#'
call_out_note = function(statement, condition, message_ok, message_fail) {

    if(statement == condition) {

        cat(":::{.callout-tip collapse='false'}\n")
        cat("## OK\n")
        cat(paste0(message_ok, collapse = "\n\n"))
        cat("\n:::\n")

    } else {

        cat(":::{.callout-warning collapse='false'}\n")
        cat("## Warning\n")
        cat(paste0(message_fail, collapse = "\n\n"))
        cat("\n:::\n")
    }
}



#' Title Note - Quarto Data Collector
#'
#' A special type of Header for Quarto that can be used when Titles depend on an executed Output from R or Python
#'
#' @param title string containing the Title
#' @param hlevel an interger giving the Header level (1-4)
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @return HTML code to be inserted in Quarto with eval='asis'
#'
#'
#' @export

title_note = function(title, hlevel) {

    if (!is.character(title)) {
        stop("Error: title must be a string.")
    }

    if (!is.numeric(hlevel)) {
        stop("Error: hlevel must be a number from 1 to 3.")
    }

    if(hlevel == 1) {

        cat(paste0("# ", title, collapse = "\n"))

    } else if(hlevel == 2) {

        cat(paste0("## ", title, collapse = "\n"))

    } else {

        cat(paste0("### ", title, collapse = "\n"))

    }


}
