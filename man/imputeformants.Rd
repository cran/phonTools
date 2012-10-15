\name{imputeformants}
\alias{imputeformants}
\title{
Impute Missing Formant Values
}
\description{
Impute missing formant values using a least-squares approximation.
}
\usage{
imputeformants (ffs, speaker, vowel)
}
\arguments{
  \item{ffs}{ a numeric vector containing formant frequency measurements for a single formant. Values should be in Hz, and missing values need to be set to 0.} 
  \item{speaker}{ a vector indicating which unique speaker produced each formant.} 
  \item{vowel}{ a vector indicating which vowel category each formant belongs to.} 
}
\details{
This function finds the least-squares approximation for each missing value based on the assumption that each formant for a given vowel differs between-speakers solely on the basis on a speaker-specific multiplicative parameter. This parameter would be most closely related to gross speaker vocal-tract length.
}
\value{
A vector containing each orignal formant frequency and imputed formant frequencies where appropriate.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

data (h95)

## Select F2 values from the tenth speaker in H95 data
## set the first 5 values to "missing"
ffs = h95$f2
ffs[h95$speaker == 10][1:5] = 0
speaker = h95$speaker
vowel = h95$vowel

## impute these missing values
imputedf2 = imputeformants (ffs, speaker, vowel)

## resulting in a very close approximation of the original values
plot (imputedf2[h95$speaker == 10], h95$f2[h95$speaker == 10])
abline (0, 1, col = 2)

}





