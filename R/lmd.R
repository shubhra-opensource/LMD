

#' Local Mean Decomposition
#'
#' @description
#' Method for finding Product Functions (PFs)
#'
#' @details
#' LMD is a method of decomposing signal into Product Functions (PFs) based on algorithm presented in Jonathan S. Smith. The local mean decomposition and its application to EEG perception data. Journal of the Royal Society Interface, 2005, 2(5):443-454
#'
#' @param signal Signal values (Numeric | vector)
#' @param INCLUDE_ENDPOINTS Whether to treat the endpoint of the signal as a pseudo-extreme point (Boolean)
#' @param MAX_SMOOTH_ITERATION Maximum number of iterations of moving average algorithm (Integer)
#' @param MAX_ENVELOPE_ITERATION Maximum number of iterations when separating local envelope signals (Integer)
#' @param ENVELOPE_EPSILON Terminate processing when obtaining pure FM signal (Double)
#' @param CONVERGENCE_EPSILON Terminate processing when modulation signal converges (Double)
#' @param MAX_NUM_PF The maximum number of PFs generated(Integer)
#'
#' @return list(pf,residue) | PFs:The decompose functions arranged from high frequency to low frequency | residue:residual component
#' @author Shubhra Prakash, \email{shubhraprakash247@@gmail.com}
#' @references \url{https://pypi.org/project/PyLMD/}
#' @keywords lmd
#' @examples
#' x=1:100
#' y = 2 / 3 * sin(x * 30) + 2 / 3 * sin(x * 17.5) + 4 / 5 *cos(x * 2)
#' plot(y,type="l")
#' lmd(y)
#' @export
#'

lmd <- function(signal,
                INCLUDE_ENDPOINTS=TRUE,
                MAX_SMOOTH_ITERATION=12,
                MAX_ENVELOPE_ITERATION=200,
                ENVELOPE_EPSILON=0.01,
                CONVERGENCE_EPSILON=0.01,
                MAX_NUM_PF=8) {

  pf = list()
  count = 1
  # until the residual function is close to a monotone function
  residue = signal
  # while ((length(pf) < MAX_NUM_PF) &&
  #        (!is_monotonous(residue)) &&
  #        (length(find_extrema(residue)) >= 5)) {
  #   component = extract_product_function(residue)
  #   residue = residue - component
  #   pf[[count]] = component
  #   count = count + 1
  #
  # }

  return(list("pf"=pf,"residue"=residue))

}



# lmd: no visible global function definition for 'find_extrema'
# lmd: no visible global function definition for'extract_product_function'











