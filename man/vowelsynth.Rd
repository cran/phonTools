\name{vowelsynth}
\alias{vowelsynth}
\title{
Vowel Synthesis
}
\description{
Create synthetic vowels using a cascade formant synthesizer. 
}
\usage{
vowelsynth (ffs = c(270, 2200, 2800, 3400, 4400), fbw = 0.06, dur = 300, f0 = c(120,100), 
fs = 10000, verify = FALSE, returnsound = TRUE) 
}
\arguments{
  \item{ffs}{A vector of center frequencies for each formant. For moving formants, a list containing two vectors may be provided, where the first vector indicates intitial values, and the final vector indicates final values. Formant frequencies should be provided in order of lowest to highest frequency.}   
  \item{fbw}{A vector of formant bandwidths for each formant. If a single value is provided, this is used for all formants. If these are not provided they are set to 6 percent of the formant center frequencies by default.} 
  \item{dur}{The desired duration of the sound, in milliseconds.}   
  \item{f0}{The desired f0 (pitch) of the sound. Optionally, a vector with initial and final f0 values may be provided.}   
  \item{fs}{The desired sampling frequency of the output sound.}   
  \item{verify}{If TRUE, the waveform and spectrogram of the created sound are plotted to allow the user to visually verify the process.}   
  \item{returnsound}{If TRUE, the sound is returned as a sound object, which can be used with several other functions included in this package. If FALSE, only a vector representing the sound wave is returned.}   
}
\details{
**Please note this function is at an early stage of development. Extended functionality will come in the near future.**

This function is a relatively bare-bones Klatt-style cascade formant synthesizer that is intended to create synthetic vowel sounds. The voice source is generated using the KLGLOTT88 method described in Klatt (1988).

If the vowels sound too 'robotic', there may be formants too close to the Nyquist frequency. Conversely, if the vowels sound too 'muffled', you may need to add more formants, and make sure they extend closer to the Nyquist frequency. 
}
\value{
A vector or sound object representing the filtered sound.
}
\references{
Klatt, D. H. (1980). Software for a cascade/parallel formant synthesizer. Journal of the Acoustical Society of America 67(3): 971-995.

Klatt, D. H. (1988). Klattalk: The conversion of English text to speech. Unpublished Manuscript. Massachusetts Institute of Technology, Cambridge, MA. Chapter 3.

}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## The following examples are based on my vowels 
#i = vowelsynth (returnsound = FALSE, f0 = c(125,105))
#a = vowelsynth (ffs = c(700, 1300, 2300, 3400, 4400), returnsound = FALSE, f0 = c(125,105))
#e = vowelsynth (ffs = c(400, 2000, 2600, 3400, 4400), returnsound = FALSE, f0 = c(125,105))
#o = vowelsynth (ffs = c(400, 900, 2300, 3400, 4400), returnsound = FALSE, f0 = c(125,105))
#u = vowelsynth (ffs = c(300, 750, 2300, 3400, 4400), returnsound = FALSE, f0 = c(125,105))

#silence = rep(0, 1000)
#vowels = c(a, silence, e, silence, i, silence, o, silence, u)

#writesound (vowels, filename = 'vowels.wav', fs = 10000)

## an example of a synthetic diphthong

#ei = vowelsynth (ffs = list(c(400, 2000, 2600, 3400, 4400), c(270, 2200, 2800, 3400, 4400)), #returnsound = FALSE, f0 = c(125,105))

#writesound (ei, fs = 10000)
}





