\name{makeFIR}
\alias{makeFIR}
\title{
Create a Digital Filter
}
\description{
Design a Finite Impulse Response (FIR) Filter.
}
\usage{
makeFIR (frequency, dB, order = 200, signal = NULL, window = 'hann', 
         verify = FALSE, interpolation = 'linear')
}
\arguments{
  \item{frequency}{The frequencies at which the frequency response of the filter will be specified. The first frequency specified must be equal to 0. The final frequency specified is assumed to be equal to fs/2.} 
  \item{dB}{The power (in decibels) of the filter at each specified frequency.} 
  \item{order}{The order of the signal.} 
  \item{signal}{If a signal is provided, it is filtered and returned.} 
  \item{window}{The type of window to be applied to the filter impulse response. Uses the windowfunc() function included in this package.}   
  \item{verify}{If TRUE, a series of plots are created to allow the user to verify that the filter is appropriate.} 
  \item{interpolation}{Determines 'linear' or 'cubic' interpolation between the specified points. Uses the interpolate() function included in this package.} 
}
\details{
Designs Type I FIR filters of odd length (even order). If an odd order is provided, 1 is added to the order. Filters are designed using the window-design method. The filter frequency response is defined at evenly-spaced locations determined by the filter order and the sampling frequency. If frequency specifications do not fall exactly on these points, the nearest appropriate location is used. This design method may lead to 'undesirable' behaviour between specified frequencies. This can be minimized by increasing the filter order and selecting an appropriate window function. 
}
\value{
If output = TRUE, the impulse response of the filter specified by the user is returned.
}
\references{
Lyons, R. G. (2004). Understanding Digital Signal Processing (2nd ed.). Prentice Hall.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

## specify a filter with an arbitrary response
#frequency = c(0, 500, 502, 5000, 5002, 7000, 7002, 11025)
#dB = c(0, 0, -50,  -50, -10,  -10, -70, -70)

## create the filter and verify that the frequency response is as desired
#testfilter = makeFIR (frequency, dB, verify = TRUE, order = 1500)
#spectralslice (testfilter, padding = 1000)


## create the filter and verify that the frequency response is as desired
#makeFIR (frequency, dB, verify = TRUE, order = 300, signal = rnorm (1400))

}





