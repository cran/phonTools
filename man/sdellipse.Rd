\name{sdellipse}
\alias{sdellipse}
\title{
Standard deviation Ellipse
}
\description{
Draw standard deviation ellipses around a group of observations.
}
\usage{
sdellipse (points, stdev = 1.96, density = .01,  
add = TRUE, show = TRUE, means = NULL, se = FALSE, ...)
}
\arguments{
  \item{points}{A matrix with two columns in which each row is a different observation from a bivariate normal distribution. Optionally, a 2 by 2 covariance matrix may be specified directly in conjunction with the means parameter.} 
  \item{stdev}{The number of standard deviations to be enclosed by the ellipse.} 
  \item{density}{The spacing between sampling points along the ellipse. A higher number results in a coarser sampling.} 
  \item{add}{If TRUE, the ellipse is added to an existing plot. If FALSE, a new plot is created.} 
  \item{show }{If FALSE, no ellipse is drawn.} 
  \item{means}{A vector of 2 means, one for each dimension. If these are specified, points is assumed to be a covariance matrix rather than a sequence of observations.} 
  \item{se}{If TRUE, a standard error (rather than standard deviation) ellipse is plotted.} 
  \item{... }{Additional arguments are passed to the internal call of lines() or plot() as appropriate.} 
}
\details{
This function may be used in 2 different ways: 1) To draw standard deviation ellipses around a set of observations (if 'means' is not specified) 2) To draw ellipses and circles on plots (if 'means' is specified).  
}
\value{
A matrix is returned where the first column indicate x-axis ellipse coordinates and the second column indicates y-axis ellipse coordinates. 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## Examples of draws from different bivariate normal distributions
## and standard deviation ellipses drawn to fit them.
#par (mfrow = c(2,2))
#draws = rmvtnorm (n = 1000, k = 2, sigma = .3)
#plot (draws)
#sdellipse (draws, stdev = 3, lwd = 3, col = 2)

#draws = rmvtnorm (n = 1000, k = 2, sigma = -.3)
#plot (draws)
#sdellipse (draws, stdev = 3, lwd = 3, col = 2)

#draws = rmvtnorm (n = 1000, k = 2, sigma = -.7)
#plot (draws)
#sdellipse (draws, stdev = 3, lwd = 3, col = 2)

#draws = rmvtnorm (n = 1000, k = 2, sigma = .7)
#plot (draws)
#sdellipse (draws, stdev = 3, lwd = 3, col = 2)

## alternatively, a covariance matrix may be specified directly. 
#par (mfrow = c(1,1))
#sdellipse (matrix(c(1,.5,.5,1),2,2), means = c(0,0), 
#add = FALSE, stdev = 1)
#sdellipse (matrix(c(1,-.5,-.5,1),2,2), means = c(0,0), 
#add = TRUE, stdev = 1)
}
