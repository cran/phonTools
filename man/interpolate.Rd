\name{interpolate}
\alias{interpolate}
\title{
Interpolation
}
\description{
Piece-wise cubic or linear spline interpolation.
}
\usage{
interpolate (y, x = 1:length(y), steps = 20, increment = -1, show = FALSE, 
output = TRUE, type = 'cubic', ...)
}
\arguments{
  \item{y}{ a vector of 'knots', between which the function will interpolate points.}
  \item{x}{ the 'x' coordinates corresponding to each knot. If not specified, the knots are assumed to be equally spaced.}
  \item{steps}{ the number of interpolating steps between each knot. Increasing this number will result in a smoother interpolation. If the knots are not equally spaced along the x-axis, the interpolated points will not be equally spaced.}
  \item{increment}{ If this is greater than 0, interpolated points are separated along the x-axis by this value. Note that if the knot locations are not multiples of this increment, there will be irregularities in the spacing of the interpolated points.}
  \item{show}{ if TRUE, the result of the interpolation is shown in a plot.}
  \item{output}{ if TRUE, the interpolated values are returned.} 
  \item{type}{ if 'cubic', a natural cubic spline interpolation is performed. If 'linear', a linear interpolation is performed.} 
  \item{...}{ additional arguments are passed to the internal call of plot if output = TRUE.}
}
\details{
By default, this function performs a 'natural' cubic spline interpolation between the points provided by the user. Optionally, a linear interpolation between the points may be carried out.
}
\value{
If output = TRUE, a dataframe with columns corresponding to the x and y columns of the interpolated points is returned.

\item{x}{ the x-axis coordinates of the inerpolated points.}
\item{y}{  the y-axis coordinates of the inerpolated points.}
}
\references{
http://en.wikipedia.org/wiki/Spline_interpolation
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
y = rnorm (10, 0, 5)
interpolate (y, output = FALSE, show = TRUE)
linear = interpolate (y, type = 'linear')
lines (linear, col = 2)
}
