\name{spectralslice}
\alias{spectralslice}
\title{
Spectral Slice
}
\description{
A function to plot the spectrum of a vector representing a sound wave. 
}
\usage{
spectralslice (sound, padding = length(sound) * 2, fs = 1,  
show = TRUE, add = FALSE, window = "kaiser", 
windowparameter = 3, zeromax = TRUE, preemphasisf = 0, type, 
line = FALSE, removeDC = TRUE, ...)
}
\arguments{
  \item{sound}{A vector representing a sound wave, or a 'sound' object.}
  \item{padding}{The amount of zero-padding desired for the analysis, in number of samples.}
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed this does not need to be specified.}
  \item{show}{If FALSE, no plot is created.}
  \item{add}{If TRUE, the spectrum is added to an existing plot. If FALSE a new one is created.}
  \item{window}{The window to be applied to the signal, applied by windowfunc(), provided in this package.}
  \item{windowparameter}{The parameter for the window to be applied to the signal, if appropriate. Passed to windowfunc().}
  \item{zeromax}{If TRUE, the maximum dB is set to 0.}
  \item{type}{The line type to be used for plotting, passes its value to the 'lty' parameter.}
  \item{preemphasisf}{Preemphasis of 3 dB per octave is added to frequencies above the specified frequency.}
  \item{line}{If TRUE, a line spectrum is created.}
  \item{removeDC}{If TRUE, the DC component is removed.}
  \item{\dots}{any additional arguments will be passed to the internal calls of 'plot' or 'lines'.}
}
\details{
This function can be used to calculate and plot short-time discrete spectra for sections of signals.  
}
\value{
A dataframe with the following elements is returned:

\item{hz}{The center frequency of the analysis bins.}
\item{dB}{The dB magnitude/power for the analysis bin.}
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
data (sound)
par (mfrow = c(2,2))

spectralslice (sound$sound[11000:11200], padding = 0, 
               main = 'few points, no padding')
spectralslice (sound$sound[11000:11600], padding = 0, 
               main = 'many points, no padding')

spectralslice (sound$sound[11000:11200], padding = 5000, 
               main = 'few points, with padding')
spectralslice (sound$sound[11000:11600], padding = 5000, 
               main = 'many points, with padding')

## line spectrum
spectralslice (sound$sound[11000:11100], padding = 0, line = TRUE)

}
