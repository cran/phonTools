\name{loadsound}
\alias{loadsound }
\alias{print.sound}
\alias{plot.sound}
\title{
Load WAV files into R
}
\description{
A function which allows WAV files to be loaded into R. 
}
\usage{
loadsound (filename)
}
\arguments{
  \item{filename}{A string indicating the file name of the WAV file to be loaded. If no filename is provided, a dialog box will open allowing the user to select a file.}
}
\details{
The function is only compatible with 8 and 16 bit WAV files. Only mono WAV files are supported. The function returns a 'sound object'. The following functions interact with 'sound' objects:


Ffilter()

FIRfilter()

loadsound()

makesound()

plot()

powertrack()

spectrogram()

vowelsynth()

writesound()
}
\references{
https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
}
\value{
An object of class 'sound', a list containing the elements:

\item{ filename }{ a vector containing the filename of the WAV file.}
\item{ fs }{ the sampling frequency of the sound. }
\item{ duration }{ the duration of the sound, in milliseconds. }
\item{ sound }{ a vector of numeric values representing the sampled sound. }
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## For example, for a file named 'filemame.wav' in the working directory
## sound = loadsound ('filename.wav')

## sound
## plot (sound)
}
