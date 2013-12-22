\name{preemphasis}
\alias{preemphasis}
\title{
Add Preemphasis
}
\description{
Creates a vowel template to be used with the PSTM() function.
}
\usage{
preemphasis (input, cutoff = 50, fs = 22050, verify = FALSE, 
coeff = 0)
}
\arguments{
  \item{input}{Either a numeric vector representing a sequence of samples taken from a sound wave or a sound object created with the loadsound() or makesound() functions.}
  \item{cutoff}{Frequencies aboe this will be given preemphasis.} 
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed, this does not need to be specified.} 
  \item{verify}{If TRUE, before and after spectra are plotted to allow the user to visually verify the process.} 
  \item{coeff}{Optionally, the single feedforward coefficient used by the filter may be specified.} 
}
\details{
Increases the spectral slope by 3 dB. per octave using a single-pole filter
}
\value{
The modified sound is returned. If a 'sound' object another 'sound' object is returned. 
}
\references{

http://www.fon.hum.uva.nl/praat/manual/Sound__Filter__pre-emphasis____.html

}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
signal = sinusoid (c(100, 200, 400, 800, 1600), fs = 4000, 
dur = 100, sum = TRUE)[,7]

preemphasis (signal, verify = TRUE, fs = 4000, cutoff = 50)
}
