\name{powertrack}
\alias{powertrack}
\title{
Waveform power track
}
\description{
Display signal power as a function of time.
}
\usage{
powertrack (sound, timestep = 2, windowlength = timestep, 
            fs = 22050, smoothing = .03, show = TRUE, ...)
}
\arguments{
  \item{sound}{A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{timestep}{Determines how far the window will be moved for each adjacent analysis, in milliseconds. The minimum permissible timestep is 0.1 milliseconds.} 
  \item{windowlength}{Determines how much of the signal is included for each analysis step, in milliseconds. By default this is set to be equal to the timestep, resulting in no overlap.} 
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed, this does not need to be specified.} 
  \item{smoothing}{This is passed to an internal call of lowess() and acts as the 'f' parameter for that function. A larger number will result in greater smoothing. For no smoothing set this to 0 or to a negative number.} 
  \item{show}{If TRUE, the track is plotted.} 
  \item{... }{Additional parameters are passed to the internal call of plot(), and used to create the figure.} 
}
\details{
First, a series of points spanning the signal, in steps equal to 'timestep', are determined. For each point, the average power within the analysis window is found. The window length is determined by the 'windowlength' parameter, and each window is centered about each analysis point. The output is expressed in decibels, where peak power is set to 0 dB.

If smoothing is set to a value greater than 0, the power track is smoothed using lowess() to minimize pitch-synchronous variation in signal power. 
}
\value{
A dataframe with the following columns:

\item{time }{ The time, in milliseconds, of each point of analysis.}
\item{power }{ The power, in decibels, at each analysis point.}
}
\references{
Lyons, R. G. (2004). Understanding Digital Signal Processing (2nd ed.). Prentice Hall.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## plot the waveform and power of a given sound.
data (sound)
# sound = loadsound()        ## run this line to use you own sound

## compare waveform and power
multiplot (2); par (mar = c(4,4,1,1));
plot (sound)
powertrack (sound)
}

