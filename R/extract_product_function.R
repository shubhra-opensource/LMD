

#' Extract Product Function
#'
#' @description
#' Method for extracting product functions
#' @param signal Signal values (Numeric | vector)
#' @param MAX_ENVELOPE_ITERATION Maximum number of iterations when separating local envelope signals (Integer)
#' @param ENVELOPE_EPSILON Terminate processing when obtaining pure FM signal (Double)
#' @param CONVERGENCE_EPSILON Terminate processing when modulation signal converges (Double)
#' @return Product Function
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @references \url{https://pypi.org/project/PyLMD/}
#' @keywords lmd
#' @examples
#' x=1:100
#' y = 2 / 3 * sin(x * 30) + 2 / 3 * sin(x * 17.5) + 4 / 5 *cos(x * 2)
#' plot(y,type="l")
#' pf=extract_product_function(y)
#' @export
#'

extract_product_function <- function(signal,MAX_ENVELOPE_ITERATION=200,ENVELOPE_EPSILON=0.01,CONVERGENCE_EPSILON=0.01) {
  s = signal
  n = length(signal)
  envelopes = list()

  component <- function(s,envelopes) {
    c2 = s
    for (e in envelopes){  # Caculate PF，using PF_i(t) = a_i(t)* s_in()
      c2 = c2 * e
    }
    return(c2)
  }

  for (temp_var in 1:MAX_ENVELOPE_ITERATION ){
    extrema = find_extrema(s)
    if (length(extrema) <= 3) break

    mean_enve=local_mean_and_envelope(s, extrema)
    m0=mean_enve$mean
    m=mean_enve$ma
    a0=mean_enve$enve
    a=mean_enve$enve_sm

    for (i in 1:length(a)){
      if(a[i]<=0) a[i] = 1 - 1e-4
    }

    #　subtracted from the original data.
    h = s - m
    #  amplitude demodulated by dividing a.
    t = h / a

    # Terminate processing when obtaining pure FM signal.
    err = sum(abs(1-a)) / n

    if (err <= ENVELOPE_EPSILON) break

    # Terminate processing when modulation signal converges.
    err = sum(abs(s-t)) / n

    if (err <= CONVERGENCE_EPSILON) break

    envelopes[[temp_var]]=a
    s = t

  }

  return (component(s,envelopes))
}
