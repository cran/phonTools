\name{resample}
\alias{resample}
\title{
Resample a Sound
}
\description{
Resample a sound at a different sampling rate.
}
\usage{
resample (sound, newfs, oldfs)
}
\arguments{
  \item{sound}{ Either a numeric vector representing a sequence of samples taken from a sound wave or a sound object created with the loadsound() or makesound() functions.}
  \item{newfs}{ The new desired sampling frequency.} 
  \item{oldfs}{ The original sampling frequency. If a 'sound' object is provided, this does not need to be specified.} 
}
\details{
The sound vector is resampled using interpolation or decimation as appropriate. The new and old sampling frequencies must be relatable by an integer, though any new sampling frequency may be achieved by first upsampling and then downsampling the signal.  
}
\value{
The resampled vector is returned. If a 'sound' object another 'sound' object is returned representing the resampled sound. 
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

data (sound)
## downsample and then upsample the sound back to its original sampling frequency
downsamped = resample (sound, 11025)
upsamped = resample (downsamped, 22050)

## compare the beginning of the waveforms for all three sounds
par (mfrow = c(3,1), mar = c(4,4,1,1))
plot (sound$sound[1:2000], type = 'l')
plot (upsamped$sound[1:2000], type = 'l', col = 2)
plot (downsamped$sound[1:1000], type = 'l', col = 4)

## the original sound, and the sound which was downsampled 
## then upsampled are very highly correlated
cor (sound$sound, upsamped$sound)

}





