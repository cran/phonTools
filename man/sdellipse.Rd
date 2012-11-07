\name{sdellipse}
\alias{sdellipse}
\title{
Standard deviation Ellipse
}
\description{
Draw standard deviation ellipses around a group of observations.
}
\usage{
sdellipse (points, stdev = 1.96, density = .1, output = FALSE, ...)
}
\arguments{
  \item{points}{A matrix with two columns in which each row is a different observation from a bivariate normal distribution.} 
  \item{stdev}{The number of standard deviations to be enclosed by the ellipse.} 
  \item{density}{The spacing between sampling points along the ellipse. A higher number results in a coarser sampling.} 
  \item{output }{If TRUE, the points used to draw the ellipse are returned.} 
  \item{... }{Additional arguments are passed to the internal call of lines().} 
}
\details{
The ellipse is drawn using lines() so a plot has to have been created for it to be visible. 
}
\value{
If output = TRUE, a matrix is returned where the first column indicate x-axis ellipse coordinates and the second column indicates y-axis ellipse coordinates. 
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## Examples of draws from different bivariate normal distributions
## and standard deviation ellipses drawn to fit them.
par (mfrow = c(2,2))
draws = rmvtnorm (n = 10000, k = 2, sigma = .3)
plot (draws)
sdellipse (draws, stdev = 3, lwd = 3, col = 2)

draws = rmvtnorm (n = 10000, k = 2, sigma = -.3)
plot (draws)
sdellipse (draws, stdev = 3, lwd = 3, col = 2)

draws = rmvtnorm (n = 10000, k = 2, sigma = -.7)
plot (draws)
sdellipse (draws, stdev = 3, lwd = 3, col = 2)

draws = rmvtnorm (n = 10000, k = 2, sigma = .7)
plot (draws)
sdellipse (draws, stdev = 3, lwd = 3, col = 2)

}





