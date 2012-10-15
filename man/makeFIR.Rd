\name{makeFIR}
\alias{makeFIR}
\title{
Create a Digital Filter
}
\description{
Design a Finite Impulse Response (FIR) Filter
}
\usage{
makeFIR (frequency, power, output = TRUE, verify = FALSE)
}
\arguments{
  \item{frequency}{ the frequencies at which the frequency response of the filter will be specified. These points must be specified only at multiples of 2.5 Hz. The first point in this vector must equal 0, and the last point is assumed to be equal to fs/2 (the Nyquist frequency).} 
  \item{power}{ the power (in decibels) of the filter at each specified frequency. The power range may not exceed 96 dB.} 
  \item{output}{ if TRUE, the filtered vector is returned.} 
  \item{verify}{ if TRUE, a series of plots are created to allow the user to verify that the filter is appropriate.} 
}
\details{
This function allows the user to create a FIR filter (specified by an impulse response) with an arbitrary frequency response using the frequency sampling method. 

If verify is TRUE, a plot is created which allows the user to inspect the performance of the function. The panels correspond to the following information:

Top left: The frequency response of the filter. This should resemble the frequency response specified by the user.

Top right: The phase response of the filter. This should be linear, save for a jumps when at the top and bottom ranges of the figure.
}
\value{
If output = TRUE, the impulse response of the filter specified by the user is returned.
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





