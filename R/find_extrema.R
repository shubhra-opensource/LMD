#' Find Extreme Points
#'
#' @description
#' Method for finding Extreme Points
#'
#' @details
#' A local extrema is the point at which a maximum or minimum value of the function in some open interval containing the point is obtained.
#' @param signal Signal values (Numeric | vector)
#' @param INCLUDE_ENDPOINTS whether to include end points or not (Boolean)
#'
#' @return Index of all extrema values (including starting and ending points)
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @keywords extrema
#' @examples
#' signal=c( 0.841471 ,0.9092974,0.14112,-0.7568025,-0.9589243)
#' find_extrema(signal)
#' @export
#'
find_extrema <- function(signal, INCLUDE_ENDPOINTS = TRUE) {
  n = length(signal)
  extremas = extreme_points(signal)
  maxima = extremas[["maxindex"]]
  minima = extremas[["minindex"]]
  all_extremas = sort(unique(c(maxima, minima)))
  if (INCLUDE_ENDPOINTS == TRUE) {
    if (length(all_extremas) == extremas[["nextreme"]]) {
      if (all_extremas[1] != 1) {
        all_extremas = c(1, all_extremas)
      }
      if (all_extremas[length(all_extremas)] != n) {
        all_extremas = c(all_extremas, n)
      }

    }
  }
  return(all_extremas)
}


