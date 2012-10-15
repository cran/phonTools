\name{h95}
\alias{h95}
\docType{data}
\title{
Hillenbrand et al. (1995) Vowel Data
}
\description{
Formant frequency, f0 and duration information for vowels collected from 139 speakers in the Hillenbrand et al. (1995) data. Speaker numbers have been modified to be uniquely identifying numbers. Data has been simplified so that only "steady state" formant frequency measures are given. Missing F2 values (n = 10) and F3 values (n = 41) have been imputed using the imputeformants() function included in this package.
}
\usage{data (h95)}
\format{
  A data frame with 1668 observations on the following 9 variables:
  \describe{
    \item{\code{type}}{ - a factor with levels \code{b} \code{g} \code{m} \code{w} representing speaker type: boy, girl, man and woman.}
    \item{\code{speaker}}{ - a numeric vector indicating a uniquely identifying speaker number.}
    \item{\code{vowel}}{ - a factor with levels \code{ae} \code{ah} \code{aw} \code{eh} \code{ei} \code{er} \code{ih} \code{iy} \code{oa} \code{oo} \code{uh} \code{uw} indicating the vowel category produced.}
    \item{\code{dur}}{ - a numeric vector indicating the duration of the vowel in milliseconds.}
    \item{\code{f0}}{ - a numeric vector indcating the vowel f0 in Hz.}
    \item{\code{f1}}{ - a numeric vector indcating the vowel F1 in Hz.}
    \item{\code{f2}}{ - a numeric vector indcating the vowel F2 in Hz.}
    \item{\code{f3}}{ - a numeric vector indcating the vowel F3 in Hz.}
  }
}
\source{
The data was created from data provided on Dr. Hillenbrand's personal website:

http://homepages.wmich.edu/~hillenbr/voweldata.html
}
\references{
Hillenbrand, J.M., Getty, L.A., Clark, M.J., and Wheeler, K. (1995). "Acoustic characteristics of American English vowels," Journal of the Acoustical Society of America, 97, 3099-3111.
}
\examples{
data(h95)
}
