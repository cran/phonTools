\name{spectralslice}
\alias{spectralslice}
\title{
Plot the Spectrum of a Vector
}
\description{
A function to plot the power spectrum of a vector representing a sound wave. 
}
\usage{
spectralslice (sound, padding = length(sound)*4, fs = 22050, output = FALSE, 
show = TRUE, color = 1, add = FALSE, xlim, ylim, alpha = 4, zeromax = TRUE, 
preemphasis = 50, type, ...)
}
\arguments{
  \item{sound}{a vector representing a sequence of samples taken at equal time intervals. Each sample should indicate the amplitude of the wave at that time interval.}
  \item{padding}{the amount of zero-padding desired for the analysis.}
  \item{fs}{the sampling frequency of the sound.}
  \item{output}{boolean. If TRUE, the frequency of bin centers and power at those centers is returned.}
  \item{show}{boolean. If FALSE, no plot is created.}
  \item{color}{set the color used for plotting the spectrum.}
  \item{add}{if TRUE, the spectrum is added to an existing plot. If FALSE a new one is created.}
  \item{xlim}{the range of frequencies to be plotted. By default the function will plot from 0 Hz to fs/2 Hz.}
  \item{ylim}{The y axis (power) range to plot.}
  \item{alpha}{the alpha parameter for the kaiser window applied to the signal.}
  \item{zeromax}{boolean. If TRUE, the maximum dB is set to 0.}
  \item{type}{the line type to be used for plotting, passes its value to the 'lty' parameter.}
  \item{preemphasis}{preemphasis of 6 dB per octave is added to frequencies above the specified frequency. For no preemphasis, set to a frequency higher than the samplling frequency.}
  \item{\dots}{any additional arguments will be passed to the internal calls of 'plot' or 'lines'.}
}
\details{
This function can be used to create spectral slices.  
}
\value{
If output is set to TRUE, a dataframe with the following elements is returned:

\item{hz}{the center frequency of the analysis bins.}
\item{amp}{the dB amplitude for the analysis bin.}

}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
data (sound)
par (mfrow = c(2,2))

spectralslice (sound$sound[1:100], padding = 0, xlim = c(0,5000), 
          main = 'few points, no padding')
spectralslice (sound$sound[1:600], padding = 0, xlim = c(0,5000), 
          main = 'many points, no padding')

spectralslice (sound$sound[1:100], padding = 5000, xlim = c(0,5000), 
          main = 'few points, with padding')
spectralslice (sound$sound[1:600], padding = 50000, xlim = c(0,5000), 
          main = 'many points, with padding')
}
