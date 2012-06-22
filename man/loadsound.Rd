\name{ loadsound}
\alias{ loadsound }
\alias{ print.sound }
\alias{ plot.sound }
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
  \item{ filename }{ a string indicating the name of the WAV file to be loaded. }
}
\details{
The function is only compatible with WAV files of 8 and 16 bits. Only mono WAV files are supported. The function returns a sound object. The functions plot() and print() are defined for sound objects. 
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
