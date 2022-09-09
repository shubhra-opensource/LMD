

#' Square Val
#'
#' @param x # An integer
#'
#' @return y: An Integer

#'
#' @examples square_val(2)
#' @author Shubhra Prakash, \email{shubhraprakash247@@gmail.com}
#' @references \url{https://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' @seealso \code{\link{utils}}
#' @keywords hplot
#' @importFrom grDevices rgb2hsv
#' @import ggplot2
#'
#' @export
square_val <- function(x=2) {
  y=x**2
  p=ggplot2::ggplot()
  return(y)

}
