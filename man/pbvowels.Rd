\name{pbvowels}
\alias{pbvowels}
\docType{data}
\title{
Peterson & Barney Vowels
}
\description{
Formant frequency and f0 information for vowels collected from 76 speakers in the Peterson & Barney (1952) data.
}
\usage{data (pbvowels)}
\format{
  A data frame with 1520 observations on the following 9 variables:
  \describe{
    \item{\code{type}}{ - a factor with levels \code{c} \code{m} \code{w} representing speaker type: children, men and women.}
    \item{\code{sex}}{ - a factor with levels \code{f} \code{m} representing speaker gender: male and female.}
    \item{\code{speaker}}{ - a numeric vector indicating a uniquely identifying speaker number.}
    \item{\code{vowel}}{ - a factor with levels \code{aa} \code{ae} \code{ah} \code{ao} \code{eh} \code{er} \code{ih} \code{iy} \code{uh} \code{uw} indicating the vowel category produced.}
    \item{\code{repetition}}{ - a numeric vector indicating the repetition number.}
    \item{\code{f0}}{ - a numeric vector indcating the vowel f0 in Hz.}
    \item{\code{f1}}{ - a numeric vector indcating the vowel F1 in Hz.}
    \item{\code{f2}}{ - a numeric vector indcating the vowel F2 in Hz.}
    \item{\code{f3}}{ - a numeric vector indcating the vowel F3 in Hz.}
  }
}
\source{
The data was created from tables provided within Praat:

http://www.fon.hum.uva.nl/praat/
}
\references{
Peterson, G.E. & Barney (1952). Control methods used in a study of the vowels. Journal of the Acoustical Society of America 24: 175-184.

Boersma, Paul & Weenink, David (2012). Praat: doing phonetics by computer [Computer program]. Version 5.3.19, retrieved 24 June 2012 from http://www.praat.org/
}
\examples{
data(pbvowels)
}
