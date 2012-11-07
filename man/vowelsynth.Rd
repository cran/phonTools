\name{vowelsynth}
\alias{vowelsynth}
\title{
Vowel Synthesis
}
\description{
Create synthetic vowels using a cascade formant synthesizer. 
}
\usage{
vowelsynth (ffs = c(270, 2200, 2800, 3400), bws, dur = 200, f0 = 120,
fs = 10000, verify = FALSE, returnsound = TRUE) 
}
\arguments{
  \item{ffs}{A vector of center frequencies for each formant. These should be provided in order of lowest to highest frequency. The defaults given are appropriate for the /i/ of an adult male.}   
  \item{bws}{A vector of formant bandwidths for each formant. If these are not provided they are set to 6 percent of the formant center frequencies by default.} 
  \item{dur}{The desired duration of the sound, in milliseconds.}   
  \item{f0}{The desired f0 (pitch) of the sound.}   
  \item{fs}{The desired sampling frequency of the output sound.}   
  \item{verify}{If TRUE, the source and vowel spectra are plotted to allow the user to visually verify the process.}   
  \item{returnsound}{If TRUE, the sound is returned as a sound object, which can be used with several other functions included in this package. If FALSE, only a vector representing the sound wave is returned.}   
}
\details{
**Please note this function is at an early stage of development. At the moment, it is only capable of creating voiced vowels with steady formant frequencies and f0 contours. Extended functionality will come in the near future.**

This function represents a bare-bones Klatt-style cascade formant synthesizer. The voice source is generated using a pulse-train method. Although f0 is steady, a small amount of flutter is included to avoid overly robotic vowels. A very small amount of gaussian noise is added to the source for the same reason. 

If the vowels sound too 'robotic', you may be specifying too many formants or there may be formants too close to the Nyquist frequency. Conversely, if the vowels sound too 'muffled', you may need to add more formants, and make sure they extend closer to the Nyquist frequency. 
}
\value{
A vector representing the filtered sound.
}
\references{
Klatt, D. H. (1980). Software for a cascade/parallel formant synthesizer. Journal of the Acoustical Society of America 67(3): 971-995.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{


## The following examples are based on my vowels 
i = vowelsynth (returnsound = FALSE)
a = vowelsynth (ffs = c(700, 1300, 2300, 3400), returnsound = FALSE)
e = vowelsynth (ffs = c(400, 2000, 2600, 3400), returnsound = FALSE)
o = vowelsynth (ffs = c(400, 800, 2300, 3400), returnsound = FALSE)
u = vowelsynth (ffs = c(300, 750, 2300, 3400), returnsound = FALSE)

silence = rep(0, 1000)
vowels = c(a, silence, e, silence, i, silence, o, silence, u)

writesound (vowels, filename = 'vowels.wav', fs = 10000)

}





