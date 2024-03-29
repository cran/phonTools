\name{peakfind}
\alias{peakfind}
\title{
Find the Peaks
}
\description{
Locate the peaks in a numeric vector.
}\usage{
peakfind (x, show = TRUE)
}
\arguments{
  \item{x}{A vector whose peaks are to be located.} 
  \item{show}{If TRUE, the vector is plotted and peaks are indicated with red triangles.} 
}
\details{
This function looks for peaks by finding elements whose value is greater than both the elements that surround it. If no peaks are found, a value of zero is returned. 
}
\value{
A vector indicating the location (position in the vector) of peaks in the vector.  
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
#data (sound)
#sound2 = sound$sound[10000:11000] 
#spectrum = spectralslice (sound2, padding = 0, output = TRUE, show = TRUE)
#peakfind (spectrum[,2])
}
