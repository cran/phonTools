\name{createtemplate}
\alias{createtemplate}
\title{
Create a Vowel Template
}
\description{
Creates a vowel template to be used with the sliding template model.
}
\usage{
createtemplate (formants, vowels)
}
\arguments{
  \item{formants}{ a matrix of formant frequencies in which each row represents a single token and each column a formant frequency for a single formant. Formant values must be normalized using the log-mean method of Nearey (1978). This may be done using the normalize function included in this package.}
  \item{vowels}{ a vector indicating the vowel category of each vowel in 'formants'. The length of this vector must equal the number of rows in 'formants'.}
}
\details{
This function finds the means location for each vowel in an n-dimensional normalized space, where each dimension corresponds to a normalized formant frequency. The covariance matrix for the formants frequencies provided is also found. This information is meant to be used in conjunction with the slidingtemplate function, included in this package. The means and covariance matrix were calculate as described in Nearey & Assmann (2007).
}
\value{
A list containing the elements:
\item{means}{ a matrix containing the mean location for each vowel category within the normalized formant space.}
\item{covariance}{ the covariance matrix for the formant frequencies provided.}
}
\references{
Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD thesis, Indiana University Linguistics Club. 


Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding template' models for indirect vowel normalization. in Experimental Approaches to Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala (Oxford University Press, Oxford), pp. 246-269.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
library (phonTools)
data (pbvowels)

normdvowels = normalize (pbvowels[,7:9], pbvowels$speaker, pbvowels$vowel)
formants = normdvowels$formants
vowels = pbvowels$vowel

createtemplate (formants, vowels)
}







