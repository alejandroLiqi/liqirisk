#' %contains% Operator
#'
#' An operator that matches the patterns on a vector of strings.
#'
#' @param strings a chraracter vector containg strings .
#' @param matchVec class character object, not vector, containing patterns.
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#' @export


# like %in%, but greps for all the patterns (rhs) within the strings (lhs)

`%contain%` = function(strings, matchVec) {

  Reduce(
     `|`,
     lapply(matchVec, function(m) {

        grepl(m,strings)

    })

   )
}
