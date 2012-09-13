\name{windowfunc}
\alias{windowfunc}
\title{
Generate a Window Function
}
\description{
Generates a window function of a given type and number of points.
}
\usage{
windowfunc (npoints = 100, type = '', parameter = -1)
}
\arguments{
  \item{npoints}{ the desired window length, in points.}
  \item{type}{ a string indicating the type of window desired. For the sake of simplicity, all window names are in lowercase. Supported types are: rectangular, hann, hamming, cosine, bartlett, gausian, and kaiser.}
  \item{parameter}{ the parameter necessary to generate the window, if appropriate. At the moment, the only windows that require parameters are the Kaiser and Gaussian windows. By default, these are set to .}
}
\details{
A window function is generated, of the type and length specified by the user. This is returned as a numeric vector.
}
\references{
http://en.wikipedia.org/wiki/Window_function
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
par (mfrow = c(1,4))
plot (windowfunc ('hann'))
plot (windowfunc ('bartlett'))
plot (windowfunc ('kaiser', parameter = 2))
plot (windowfunc ('kaiser', parameter = 6))
}







