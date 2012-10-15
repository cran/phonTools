\name{vowelplot}
\alias{vowelplot}
\title{
Plot Vowels
}
\description{
A flexible function which can be used to create a wide variety of vowel plots.  
}
\usage{
vowelplot(f1s, f2s, labels = 0, xrange = NULL, yrange = NULL, meansOnly = FALSE,
ellipses = FALSE, ellipsesd = 1.96, add = FALSE, pointType = 0,
colors = NULL, logaxes = "", grid = FALSE, gridInterval = c(200, 500), 
gridStart = c(0, 0), defaultPlot = TRUE, alternateAxes = FALSE, ...)
}
\arguments{
  \item{f1s}{a numeric vector indicating vowel F1 frequencies.}
  \item{f2s}{a numeric vector indicating vowel F1 frequencies.}
  \item{labels}{a vector with labels for vowels. Must be provided for any category-dependent differences in plotting.}
  \item{xrange}{allows the user to set the x axis range for the plot.}
  \item{yrange}{allows the user to set the y axis range for the plot.}
  \item{meansOnly}{boolean. If TRUE only category means are plotted (labels must be provided).}
  \item{ellipses}{boolena. If TRUE standard deviation ellipses are drawn (one per category as indicated by label vector).}
  \item{ellipsesd}{a number indicating the number of standard deviations ellipses will enclose.}
  \item{add}{boolean. If TRUE vowels are plotted on existing figure, if FALSE a new one is created.}
  \item{pointType}{kinds of points to use determined by 'pch' value. If specified it overrides text labels. It cycles through the list given if number of point types are less than number of categories.}
  \item{colors}{colors to use for different categories. If specified this overrides automatic colors. It cycles through the list given if number of colors are less than number of categories.}
  \item{logaxes}{by default linear axes are used. For log axes set to 'xy'.}
  \item{grid}{boolean. If TRUE a grid is drawn on the plot.}
  \item{gridInterval}{2-tuple. Determines the spacing between grid lines. The first number determines the x-axis spacing, the second determines the y-axis spacing.}
  \item{gridStart}{2-tuple. Determines where the grid lines begin. The first number determines x-axis start, the second determines the y-axis start.}
  \item{defaultPlot}{boolean. If this is set to true, the function plots using pre-determined values. If FALSE, the user has almost complete control over the internal call of 'plot'.}
  \item{alternateAxes}{boolean. If TRUE, F1 is plotted on the y axis and F2 on the x axis with the origin in the top right corner. By default F1 is plotted on the x axis and F2 on the y axis with the origin in the bottom left corner.}
  \item{\dots}{additional arguments are passed to the internal call of 'plot'.}
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
data (pb52)

## the top row of the figure created with the following code highlights differences
## in general layout. The bottom row highlights differences in presentation of data points
## these plots represent only a subset of possible configurations
par (mfrow = c(2,4), mar = c(4.2,4.2,1,1))

## standard layout with linear axes
vowelplot (pb52$f1, pb52$f2, pb52$vowel)

## standard layout with log axes
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy')

## alternate layout with log axes
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', alternateAxes = TRUE)

## standard layout with log axes and grid lines
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', grid = TRUE)

## points rather than text labels for vowels
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', pointType = 16)

## points with standard deviation ellipses
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', pointType = 16,
           ellipses = TRUE)

## category means only 
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', meansOnly = TRUE)

## category means only with standard deviation ellipses
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', meansOnly = TRUE,
           ellipses = TRUE)
}
