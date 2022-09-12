#' Extreme Points and Zero Crossings
#'
#' @description
#' Finding Local Extrema and Zero-crossings
#'
#' @details
#' A local extrema is the point at which a maximum or minimum value of the function in some open interval containing the point is obtained. A zero-crossing is a point where the sign of a mathematical function changes.
#'
#' @param y Signal values (Numeric | vector)
#' @param ndata Number of data points (Integer)
#' @return Index values for minima, maxima, zero crossings, number of extremas and zero crossing
#' @references \url{https://cran.r-project.org/web/packages/EMD/index.html}
#' @keywords local minima maxima extrema
#' @examples
#' x=1:100
#' y = 2 / 3 * sin(x * 30) + 2 / 3 * sin(x * 17.5) + 4 / 5 *cos(x * 2)
#' extreme_points(y)
#' @export
#'
extreme_points <- function(y, ndata = length(y)) {
  # Courtesy: Package‘EMD’
  ndatam1 = ndata - 1
  minindex <- maxindex <- NULL; nextreme <- 0; cross <- NULL; ncross <- 0

  z1 <- sign(diff(y))
  index1 <- seq(1, ndatam1)[z1 != 0]; z1 <- z1[z1 != 0]

  if (!(is.null(index1) || all(z1==1) || all(z1==-1))) {

    index1 <- index1[c(z1[-length(z1)] != z1[-1], FALSE)] + 1
    z1 <- z1[c(z1[-length(z1)] != z1[-1], FALSE)]

    nextreme <- length(index1)

    if(nextreme >= 2)
      for(i in 1:(nextreme-1)) {
        tmpindex <- index1[i]:(index1[i+1]-1)
        if(z1[i] > 0) {
          tmpindex <- tmpindex[y[index1[i]] == y[tmpindex]]
          maxindex <- rbind(maxindex, c(min(tmpindex), max(tmpindex)))
        } else {
          tmpindex <- tmpindex[y[index1[i]] == y[tmpindex]]
          minindex <- rbind(minindex, c(min(tmpindex), max(tmpindex)))
        }
      }

    tmpindex <- index1[nextreme]:ndatam1
    if(z1[nextreme] > 0) {
      tmpindex <- tmpindex[y[index1[nextreme]] == y[tmpindex]]
      maxindex <- rbind(maxindex, c(min(tmpindex), max(tmpindex)))
    } else {
      tmpindex <- tmpindex[y[index1[nextreme]] == y[tmpindex]]
      minindex <- rbind(minindex, c(min(tmpindex), max(tmpindex)))
    }

    ### Finding the index of zero crossing

    if (!(all(sign(y) >= 0) || all(sign(y) <= 0) || all(sign(y) == 0))) {
      index1 <- c(1, index1)
      for (i in 1:nextreme) {
        if (y[index1[i]] == 0) {
          tmp <- c(index1[i]:index1[i+1])[y[index1[i]:index1[i+1]] == 0]
          cross <- rbind(cross, c(min(tmp), max(tmp)))
        } else
          if (y[index1[i]] * y[index1[i+1]] < 0) {
            tmp <- min(c(index1[i]:index1[i+1])[y[index1[i]] * y[index1[i]:index1[i+1]] <= 0])
            if (y[tmp] == 0) {
              tmp <- c(tmp:index1[i+1])[y[tmp:index1[i+1]] == 0]
              cross <- rbind(cross, c(min(tmp), max(tmp)))
            } else
              cross <- rbind(cross, c(tmp-1, tmp))
          }
      }

      if (any(y[index1[nextreme+1]] * y[index1[nextreme+1]:ndata] <= 0)) {
        tmp <- min(c(index1[nextreme+1]:ndata)[y[index1[nextreme+1]] * y[index1[nextreme+1]:ndata] <= 0])
        if (y[tmp] == 0) {
          tmp <- c(tmp:ndata)[y[tmp:ndata] == 0]
          cross <- rbind(cross, c(min(tmp), max(tmp)))
        } else
          cross <- rbind(cross, c(tmp-1, tmp))
      }
      ncross <- nrow(cross)
    }
  }

  list(minindex=minindex, maxindex=maxindex, nextreme=nextreme, cross=cross, ncross=ncross)
}
