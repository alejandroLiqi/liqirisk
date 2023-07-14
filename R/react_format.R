#' Liqi Reactable General Theme
#'
#' Contains the parameters and colour for a standarized table theme.
#'
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#' @examples
#'library(liqirisk)
#'library(reactable)
#'reactable(iris, theme = react_format,
#'          highlight = TRUE, striped = TRUE, filterable = TRUE, resizable = TRUE, wrap = FALSE)
#'
#' @returns an object with lists to pass into the reactable function or options.
#' @export
#'
#' @importFrom reactable reactableTheme
#'
react_format = reactable::reactableTheme(

  tableStyle = list(fontWeight = 400, fontSize = 13),
  headerStyle = list(backgroundColor = "#000729", color = '#ffbe00', fontWeight = 300, fontSize = 14),

  backgroundColor = "#ffffff",
  borderColor = "#dfe2e5",
  stripedColor = "#f6f6f6",
  highlightColor = "#DCE4F2",

  cellPadding = "8px 12px",
  style = list(fontFamily = 'Open Sans'),

  searchInputStyle = list(width = "100%"),

  pageButtonHoverStyle = list(backgroundColor = "#ffbe00", color = 'white'),
  pageButtonActiveStyle = list(backgroundColor = "#ffbe00", color = 'white'),

  selectStyle = list(backgroundColor = "#ffbe00", color = 'white')

)






#' Liqi Input reactable
#'
#' Contains the parameters and colour for a standarized table theme.
#'
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#' @examples
#'library(liqirisk)
#'library(reactable)
#'reactable(iris, theme = liqi_input_reactable,
#'          highlight = TRUE, striped = TRUE, filterable = TRUE, resizable = TRUE, wrap = FALSE)
#'
#' @returns an object with lists to pass into the reactable function or options.
#' @export
#'
#' @import reactable
#' @importFrom reactable reactableTheme
#'
liqi_input_reactable =
    reactable::reactableTheme(
        color = '#000729',
        tableBodyStyle = list(boderWidth = 0.15, borderColor = '#f8f9fa'),
        tableStyle = list(fontSize = 11, borderRadius = 8, boderWidth = 0.15, borderColor = '#f8f9fa'),
        headerStyle = list(height = 30, align = 'center', borderRadius = 0, fontSize = 12, background = '#FFBE00', borderWidth = 0.15, color = '#fff', cursor = "pointer", lineHeight = 3),
        cellStyle = list(borderColor = "#f8f9fa")
    )


#' Liqi Output reactable
#'
#' Contains the parameters and colour for a standarized table theme.
#'
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#' @examples
#'library(liqirisk)
#'library(reactable)
#'reactable(iris, theme = liqi_output_reactable,
#'          highlight = TRUE, striped = TRUE, filterable = TRUE, resizable = TRUE, wrap = FALSE)
#'
#' @returns an object with lists to pass into the reactable function or options.
#' @export
#'
#' @importFrom reactable reactableTheme
liqi_output_reactable =
    reactable::reactableTheme(
        color = '#000729',
        tableBodyStyle = list(boderWidth = 0.15, borderColor = '#f8f9fa'),
        tableStyle = list(fontSize = 11, borderRadius = 8, boderWidth = 0.15, borderColor = '#f8f9fa'),
        headerStyle = list(height = 30, align = 'center', borderRadius = 0, fontSize = 12, background = '#000729', borderWidth = 0.15, color = '#fff', cursor = "pointer", lineHeight = 3),
        cellStyle = list(borderColor = "#f8f9fa")
    )
