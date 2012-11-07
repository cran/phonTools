\name{FIRfilter}
\alias{FIRfilter}
\title{
Perform Digital Filtering
}
\description{
Finite Impulse Response (FIR) filtering of vectors.
}
\usage{
FIRfilter (sound, fir, output = TRUE, verify = FALSE)
}
\arguments{
  \item{sound}{A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{fir}{An impulse response representing the digital filter.} 
  \item{output}{If TRUE, the filtered sound is returned.} 
  \item{verify}{If TRUE a series of plots are created which allow the user to verify the input and outputs of the function.} 
}
\details{
This function performs FIR filtering by convolving the signal and the impulse response of the filter, returning only the relevant samples. The particular sampling frequency of the signal does not affect the output sound, but must be taken account of when creating the impulse response of the filter. An appropriate FIR impulse response can be created with the createFIR() function included in this package.

If verify is TRUE, a plot is created which allows the user to inspect the performance of the function. The panels correspond to the following information:

Top left: The original sound.

Top right: The frequency response of the filter.

Bottom left: The filtered sound.

Bottom right: An overlay of the filtered sound and the frequency response of the filter.
}
\value{
If output = TRUE, the filtered vector is returned.
}
\references{
Lyons, R. G. (2004). Understanding Digital Signal Processing (2nd ed.). Prentice Hall.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## specify a filter with an arbitrary response
frequency = c(0, 500, 502.5, 5000, 5002.5, 7000, 7002.5, 11025)
power =     c(0, 0,   -50, -50, -10, -10, -70, -70)

## create the filter and verify that the frequency response is as desired
testfilter = makeFIR (frequency, power, verify = TRUE)

## create a vector of random noise
noise = rnorm (2000, 0, 200)

## filter the sound using the verify parameter in order to 
## verify that the filtered sound has the desired spectral envelope
snd = FIRfilter (noise, testfilter, verify = TRUE)

}





