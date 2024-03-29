\name{resample}
\alias{resample}
\title{
Resample a Sound
}
\description{
Resample using sinc interpolation.
}
\usage{
resample (sound, newfs, oldfs, precision = 50, filterorder = 200, synthfilter = FALSE)
}
\arguments{
  \item{sound}{Either a numeric vector representing a sequence of samples taken from a sound wave or a sound object created with the loadsound() or makesound() functions.}
  \item{newfs}{The new desired sampling frequency.} 
  \item{oldfs}{The original sampling frequency. If a 'sound' object is provided, this does not need to be specified.} 
  \item{precision}{The number of samples before and after the current point to be used for interpolation.} 
  \item{filterorder}{The number of taps to be used for the low-pass FIR filters used, where appropriate.} 
  \item{synthfilter}{If TRUE, synthfilter() is used for filtering.} 
}
\value{
The resampled vector is returned. If a 'sound' object is passed, the resampled sound is returned as an object. 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
#data (sound)
## downsample and then upsample the sound back to 
## its original sampling frequency
#downsamped = resample (sound, 11025)
#upsamped = resample (downsamped, 22050)

# compare a part of the waveforms for all three sounds
#par (mfrow = c(3,1), mar = c(4,4,1,1))
#plot (sound$sound[1:14000], type = 'l')
#plot (upsamped$sound[1:14000], type = 'l', col = 2)
#plot (downsamped$sound[1:7000], type = 'l', col = 4)
}

