\name{writesound}
\alias{writesound}
\title{
Write out a WAV file
}
\description{
Generate a WAV file from a numeric vector.
}
\usage{
writesound (samples, filename = '', fs = 22050)
}
\arguments{
  \item{samples }{ a numeric vector representing samples values of the sound wave.} 
  \item{filename }{ a string indicating the desired output file name.} 
  \item{fs }{ the desired output sampling frequency.} 
}
\details{
This function generates single channel (mono), 16-bit WAV sound files at a desired sampling frequency. If a 'sound' object is passed, the filename and sampling frequency do not need to be set. If a filename is not set, the sound created is called 'sound.wav' by default.
}
\references{
https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## generate a sine wave with a frequency of 1000 Hz
## sampled at a frequency of 10000 Hz
x = seq (0,.1, 1/10000)
snd = sin (2*pi*1000*x)
plot (snd[1:100], type = 'b')

## write out sine wave as a WAV file
writesound (snd, filename = '1khz.wav', fs = 10000)

}





