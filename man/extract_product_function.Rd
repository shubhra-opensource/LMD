% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_product_function.R
\name{extract_product_function}
\alias{extract_product_function}
\title{Extract Product Function}
\usage{
extract_product_function(
  signal,
  max_envelope_iteration = 200,
  envelope_epsilon = 0.01,
  convergence_epsilon = 0.01
)
}
\arguments{
\item{signal}{Signal values (Numeric | vector)}

\item{max_envelope_iteration}{Maximum number of iterations when separating local envelope signals (Integer)}

\item{envelope_epsilon}{Terminate processing when obtaining pure FM signal (Double)}

\item{convergence_epsilon}{Terminate processing when modulation signal converges (Double)}
}
\value{
Product Function
}
\description{
Method for extracting product functions
}
\examples{
x=1:100
y = (2 / 3 )* sin(x * 30) + (2 / 3) * sin(x * 17.5) + (4 / 5) *cos(x * 2)
plot(y,type="l")
pf=extract_product_function(y)
}
\references{
\url{https://pypi.org/project/PyLMD/}
}
\author{
Shubhra Prakash, \email{shubhraprakash279@gmail.com}
}
\keyword{lmd}
