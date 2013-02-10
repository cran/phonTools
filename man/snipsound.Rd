\name{snipsound}
\alias{snipsound}
\title{
Snip a Sound Object
}
\description{
Select a subsection of a sound object
}
\usage{
snipsound (sound, output = TRUE)
}
\arguments{
  \item{sound}{ A 'sound' object to be truncated.} 
  \item{output}{ If TRUE, the snipped 'sound' object is returned.} 
}
\details{
The input 'sound' object is plotted and the user must click on two points on the plot. A new 'sound' object is then created and plotted, containing only the samples in between the two designated points. 

The location of the click is accurate to within 1/1000 of the duration of the sound. 
}
\value{
A 'sound' object which interacts with several functions included in this package.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

data (sound)

## the example below is commented because examples cannot 
## require user interaction. simply uncomment the lines below
## and select two points on the plot. 
# clipped = snipsound (sound)
# spectrogram (clipped)
# spectralslice (clipped)

}





