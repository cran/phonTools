\name{spectralslice}
\alias{spectralslice}
\title{
Plot the Spectrum of a Vector
}
\description{
A function to plot the magnitude spectrum of a vector representing a sound wave. 
}
\usage{
spectralslice (sound, padding = length(sound)*2, fs = 22050, output = FALSE, 
show = TRUE, color = 1, add = FALSE, xlim, ylim, window = 'kaiser', 
windowparameter = 4, zeromax = TRUE, preemphasis = 50000, type, xlab, pwr = TRUE, ...)
}
\arguments{
  \item{sound}{A vector representing a sequence of samples taken at equal time intervals. Each sample should indicate the amplitude of the wave at that time interval.}
  \item{padding}{The amount of zero-padding desired for the analysis.}
  \item{fs}{The sampling frequency of the sound.}
  \item{output}{If TRUE, the frequency of bin centers and power at those centers is returned.}
  \item{show}{If FALSE, no plot is created.}
  \item{color}{Set the color used for plotting the spectrum.}
  \item{add}{If TRUE, the spectrum is added to an existing plot. If FALSE a new one is created.}
  \item{xlim}{The range of frequencies to be plotted. By default the function will plot from 0 Hz to fs/2 Hz.}
  \item{ylim}{The y axis (power/magnitude) range to plot.}
  \item{window}{The window to be applied to the signal, applied by windowfunc(), provided in this package.}
  \item{windowparameter}{The parameter for the window to be applied to the signal, if appropriate. Passed to windowfunc().}
  \item{zeromax}{If TRUE, the maximum dB is set to 0.}
  \item{type}{The line type to be used for plotting, passes its value to the 'lty' parameter.}
  \item{preemphasis}{Preemphasis of 6 dB per octave is added to frequencies above the specified frequency. For no preemphasis, set to a frequency higher than the sampling frequency.}
  \item{xlab}{the x-axis label may be specified if desired.}
  \item{pwr}{If TRUE, a power spectrum is calculated, if FALSE, a magnitude spectrum is calculated.}
  \item{\dots}{any additional arguments will be passed to the internal calls of 'plot' or 'lines'.}
}
\details{
This function can be used to calculate and plot short-time frequency spectra for sections of signals.  
}
\value{
If output is set to TRUE, a dataframe with the following elements is returned:

\item{hz}{The center frequency of the analysis bins.}
\item{amp}{The dB amplitude for the analysis bin.}

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
