\name{interpolate}
\alias{interpolate}
\title{
Cubic Spline Interpolation
}
\description{
Piece-wise 'natural' spline interpolation.
}
\usage{
interpolate (y, x = 1:length(y), steps = 20, show = FALSE, output = TRUE, ...)
}
\arguments{
  \item{y}{ a vector of 'knots', between which the function will interpolate points.}
  \item{x}{ the 'x' coordinates corresponding to each knot. If not specified, the knots are assumed to be equally spaced.}
  \item{steps}{ the number of interpolating steps between each knot. Increasing this number will result in a smoother interpolation.}
  \item{show}{ if TRUE, the result of the interpolation is shown in a plot.}
  \item{output}{ if TRUE, the interpolated values are returned.} 
  \item{...}{ additional arguments are passed to the internal call of plot if output = TRUE.}
}
\details{
This function performs a 'natural' cubic spline interpolation between the points provided by the user. 
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
}
