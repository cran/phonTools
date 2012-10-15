\name{sdellipse}
\alias{sdellipse}
\title{
Standard deviation Ellipse
}
\description{
Draw standard deviation ellipses for a bivariatse normal distribution.
}
\usage{
sdellipse (points, stdev = 1.96, density = .1, output = FALSE, ...)
}
\arguments{
  \item{points }{ a matrix with two columkns in which each row is a different observation from a bivariate normal distribution.} 
  \item{stdev }{ the number of standard deviations to be enclosed by the ellipse.} 
  \item{density }{ the spacing between sampling points along the ellipse. A higher number results in a coarser sampling.} 
  \item{output }{ if TRUE, the points used to draw the ellipse are returned.} 
  \item{... }{ additional arguments are passed to the internal call of lines().} 
}
\details{
Given a matrix with two columns in which rows represent observations from a bivariate normal distribution, an ellipse enclosing a certain number of standard deviations of this distribution are returned.   
}
\value{
A matrix where the first column indicate x-axis ellipse coordinates and the second column indicates y-axis ellipse coordinates. 
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





