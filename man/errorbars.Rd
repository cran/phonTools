\name{errorbars}
\alias{errorbars}
\alias{errorbar}
\title{
Error bars.
}
\description{
This functions adds error bars to a plot or, optionally, plots points and added error bars. 
}
\usage{
errorbars (x, y, top, bottom = top, length = .2, add = TRUE, ...)
}
\arguments{
  \item{x}{X axis positions.} 
  \item{y}{Y axis positions.} 
  \item{top}{Top error bar length.} 
  \item{bottom}{Bottom error bar length.} 
  \item{length}{The length of the horizontal bars.} 
  \item{add}{If TRUE, error bars are added to existing plot.} 
  \item{...}{Additional arguments go to the internal call of arrows() which draws the error bars.} 
}
\references{
Inspired by a similar function first seen here:

http://monkeysuncle.stanford.edu/?p=485
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}

\examples{

## add bars to existing plots 
#plot (1:10, 1:10, col = 4, pch = 18)
#errorbars (1:10, 1:10, .5, col = 4)

## or create a new plot
#errorbars (1:10, 1:10, 2, add = FALSE)

}


