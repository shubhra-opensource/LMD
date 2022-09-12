




#' Local Mean and Envelope
#'
#' @description
#' Method for finding Local Mean and Envelope
#' @param signal Signal values (Numeric | vector)
#' @param extrema indexes for extreme values#'
#' @return mean, evelope and smoothed mean and envelope values
#' @author Shubhra Prakash, \email{shubhraprakash247@@gmail.com}
#' @references \url{https://pypi.org/project/PyLMD/}
#' @keywords Local Mean and Envelope
#' @examples
#' signal = sin(1:10)
#' extrema = c(1 , 2,  5,  8, 10)
#' local_mean_and_envelope(signal, extrema)
#' @export
#'
local_mean_and_envelope <- function(signal, extrema) {
  browser()
  n = length(signal)
  k = length(extrema)
  stopifnot(1 < k)
  stopifnot(k <= n)
  # construct square signal
  mean = c()
  enve = c()
  prev_mean = (signal[extrema[1]] + signal[extrema[2]]) / 2
  prev_enve = abs(signal[extrema[1]] - signal[extrema[2]]) / 2
  e = 2
  for (x in 1:n) {
    if ((x == extrema[e]) && ((e + 1) < k)) {
      next_mean = (signal[extrema[e]] + signal[extrema[e + 1]]) / 2
      mean[length(mean) + 1] = ((prev_mean + next_mean) / 2)
      prev_mean = next_mean
      next_enve = abs(signal[extrema[e]] - signal[extrema[e + 1]]) / 2
      enve[length(enve) + 1] = ((prev_enve + next_enve) / 2)
      prev_enve = next_enve
      e = e + 1

    } else{
      mean[length(mean) + 1] = prev_mean
      enve[length(enve) + 1] = prev_enve

    }
  }# smooth square signal
  window = floor(max(diff(extrema)) / 3)

  return(
    list(
      "mean" = mean,
      "ma" = moving_average_smooth(mean, window),
      "enve" = enve,
      moving_average_smooth(enve, window)
    )
  )

}





signal=c(0.841471 ,  0.9092974,  0.14112  , -0.7568025, -0.9589243,
           -0.2794155,  0.6569866,  0.9893582,  0.4121185, -0.5440211)

extrema=c(0, 1, 4, 7, 9)

local_mean_and_envelope(signal, extrema)

















# }
