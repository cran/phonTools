\name{sound}
\alias{sound}
\docType{data}
\title{
Sound object
}
\description{
An example of a sound object, the phrase 'this is a spectrogram', produced by the author of this package. This sound object may be inspected with spectrogram() and plotted with plot(). Sections of the individual samples representing the waveform (found in the "sound" element of a sound object) can be passed to spectralslice() to see a spectral slice.
}
\usage{data (sound)}
\format{
This sound object has the same properties as all sound objects. These may be inspected by using the print() function.
}
\examples{
data (sound)

sound
plot (sound)
spectrogram (sound, pause = FALSE)

output = spectrogram (sound, output = TRUE, show = FALSE, pause = FALSE)
plot (output, pause = FALSE)
}
