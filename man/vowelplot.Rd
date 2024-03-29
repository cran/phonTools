\name{vowelplot}
\alias{vowelplot}
\title{
Plot Vowels
}
\description{
A flexible function that can create a wide variety of vowel plots (including IPA symbols).  
}
\usage{
vowelplot (f1s, f2s, labels = 0, xrange = NULL, yrange = NULL, 
meansOnly = FALSE, ellipses = FALSE, ellipsesd = 1.96, add = FALSE, 
pointType = 0, colors = NULL, logaxes = '', defaultPlot = TRUE, 
alternateAxes = FALSE, xsampa = FALSE, ...)
}
\arguments{
  \item{f1s}{A numeric vector indicating vowel F1 frequencies.}
  \item{f2s}{A numeric vector indicating vowel F2 frequencies.}
  \item{labels}{A vector with labels for vowels. Must be provided for any category-dependent differences in plotting. If x-sampa labels are given IPA symbols may be plotted.}
  \item{xrange}{Allows the user to set the x axis range for the plot.}
  \item{yrange}{Allows the user to set the y axis range for the plot.}
  \item{meansOnly}{If TRUE, only category means are plotted (labels must be provided).}
  \item{ellipses}{If TRUE, standard deviation ellipses are drawn (one per category as indicated by label vector).}
  \item{ellipsesd}{A number indicating the number of standard deviations ellipses will enclose.}
  \item{add}{If TRUE, vowels are plotted on existing figure. If FALSE, a new one is created.}
  \item{pointType}{Kinds of points to use determined by 'pch' value. If specified it overrides text labels. IPA symbols may be plotted by finding appropriate values using the pickIPA() function included in this package.}
  \item{colors}{Colors to use for different categories. If specified this overrides automatic colors. It cycles through the list given if number of colors are less than number of categories.}
  \item{logaxes}{Linear axes are used by default. For log axes set to 'xy'.}
  \item{defaultPlot}{If TRUE, the function plots using pre-determined values. If FALSE, the user has almost complete control over the internal call of 'plot'.}
  \item{alternateAxes}{If TRUE, F1 is plotted on the y axis and F2 on the x axis with the origin in the top right corner. By default F1 is plotted on the x axis and F2 on the y axis with the origin in the bottom left corner.}
  \item{xsampa}{If TRUE, the labels vector given to the function is assumed to be specified in x-sampa and IPA symbols are used to plot using the xsampatoIPA() function included in this package. If this is set to TRUE and the 'labels' input is not in x-sampa, the symbols will be wrong.}
  \item{\dots}{Additional arguments are passed to the internal call of 'plot'.}
}
\details{
*** This function has been deprecated and is no longer being developed. I recommend using vplot() instead, which is more flexible and gives the user more control over plotting. ***

This function now includes functionality to easily generate vowel plots using IPA symbols. This relies on category labels being specified in x-sampa. Alternatively, the required plotting values for IPA symbols may be selected using the pickIPA() function included in this package, and then passed to the 'pointType' parameter.

There may be issues when exporting figures to PDF using IPA font. Exporting plots directly as images works 'out of the box'.
}
\references{
http://en.wikipedia.org/wiki/X-SAMPA
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## A few examples of some vowel plots. 

#data (pb52)
#par (mfrow = c(1,4), mar = c(4.2,4.2,1,1))

# standard layout with linear axes
#vowelplot (pb52$f1, pb52$f2, pb52$vowel, xsampa = TRUE)

# alternate layout with log axes
#vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', 
#alternateAxes = TRUE, xsampa = TRUE)

# category means only 
#vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', 
#meansOnly = TRUE, xsampa = TRUE)

# category means only with standard deviation ellipses
#vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', meansOnly = TRUE,
#           ellipses = TRUE, xsampa = TRUE)
}
