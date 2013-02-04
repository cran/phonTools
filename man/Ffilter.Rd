\name{Ffilter}
\alias{Ffilter}
\title{
Formant Filtering
}
\description{
Perform cascade formant filtering of sounds.
}
\usage{
Ffilter (sound, ffs, bwp = 0.06, fs = 22050, verify = FALSE)
}
\arguments{
  \item{sound}{A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{ffs}{A list of vectors of initial and final center frequencies for each formant. Each vector should contain formant frequencies in order of lowest to highest frequency. If only a single vector is provided, formants will remain stable throughout.} 
  \item{bwp}{A vector of formant bandwidths for each formant.} 
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed, this does not need to be specified.} 
  \item{verify}{If TRUE, before and after spectra are plotted to allow the user to visually verify the process.} 
}
\details{
This function allows the user to specify one or more formant filters and to pass a signal through said filters. This may be used to create synthetic speech sounds or to modify existing sounds as desired. The given signal is passed through the formant filters in reverse order. Filter bandwidths specify the distance between formant center frequencies and the point at which the output will be 3 dB below peak energy.

Filter bandwidths may be provided in Hz, or as a percentage of the formant frequencies. To set these as a percent of formant frequencies, all values must be less than 1. If these are not provided they are set to 6 percent of the formant center frequencies by default. If only one value is provided, this is assumed to be the desired value for all formants.
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

## pass this through one formant filters
## two static formants
filtered1 = Ffilter (sound, ffs = c(4000,7000), bw = 500, verify = FALSE)
## a single moving formant
filtered2 = Ffilter (sound, ffs = list(3000,8000), bw = 500, verify = FALSE)
## two moving formants
filtered3 = Ffilter (sound, ffs = list(c(5000,4000),c(8000,1000)), bw = 500, verify = FALSE)

## inspect the results
par (mfrow = c(1,3), mar = c(4,4,1,1))
spectrogram (filtered1, maxfreq = 11025, pause = FALSE)
spectrogram (filtered2, maxfreq = 11025, pause = FALSE)
spectrogram (filtered3, maxfreq = 11025, pause = FALSE)

}