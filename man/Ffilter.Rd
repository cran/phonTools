\name{Ffilter}
\alias{Ffilter}
\title{
Formant Filtering
}
\description{
Perform cascade formant filtering of sounds.
}
\usage{
Ffilter (sound, ffs, bws, fs = 22050, verify = FALSE)
}
\arguments{
  \item{sound}{A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{ffs}{A vector of center frequencies for each formant. These should be provided in order of lowest to highest frequency.} 
  \item{bws}{A vector of formant bandwidths for each formant. If these are not provided they are set to 6 percent of the formant center frequencies by default.} 
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed, this does not need to be specified.} 
  \item{verify}{If TRUE, before and after spectra are plotted to allow the user to visually verify the process.} 
}
\details{
This function allows the user to specify one or more formant filters and to pass a signal through said filters. This may be used to create synthetic speech sounds or to modify existing sounds as desired. The given signal is passed through the formant filters in reverse order. Filter bandwidths specify the distance between formant center frequencies and the point at which the output will be 3 dB below peak energy.
}
\value{
A vector representing the filtered sound.
}
\references{
Klatt, D. H. (1980). Software for a cascade/parallel formant synthesizer. Journal of the Acoustical Society of America 67(3): 971-995.

http://www.fon.hum.uva.nl/praat/manual/Sound__Filter__one_formant____.html
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## Generate half a second of white noise
sound = rnorm (11025, 0, 1)

## pass this through one formant filter
sound = rnorm (11025, 0, 1)
filtered = Ffilter (sound, ffs = 5000, bw = 500, verify = TRUE)

## and see that the peaks, and -3 dB points fall where expected. 
spectralslice (filtered, pwr = FALSE)
spectralslice (filtered, ylim = c(-10, 0), xlim = c(4000,6000))
abline (h = c(0, -3), col = 2, lwd = 2)
abline (v = c(4500, 5000, 5500), col = 2, lwd = 2)

}
