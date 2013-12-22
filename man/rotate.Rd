\name{rotate}
\alias{rotate}
\title{
Rotate 
}
\description{
Rotate 2D and complex-valued observations.
}
\usage{
rotate (xy, angle, degrees = FALSE)
}
\arguments{
  \item{xy}{Either, a vector of complex-valued observations, or a matrix with 2 columns and any number of rows.} 
  \item{angle}{The desired angle of rotation, in radians.} 
  \item{degrees}{If degrees are provided instead of radians, this should be set to TRUE.} 
}
\details{
The output is of the same kind as the input. 
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## rotate points in a 2D space 
mat = cbind (1:100, 100:1)
rotate (mat, pi/2)

## rotate complex-valued numbers
complx = complex (real = mat[,1], imaginary = mat[,2])
rotate (complx, pi/2)

}





