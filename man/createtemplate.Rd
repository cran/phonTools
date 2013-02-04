\name{createtemplate}
\alias{createtemplate}
\alias{plot.template}
\title{
Create a Vowel Template
}
\description{
Creates a vowel template to be used with the PSTM() function.
}
\usage{
createtemplate (formants, vowels)
}
\arguments{
  \item{formants}{A matrix of formant frequencies in which each row represents a single token and each column a formant frequency for a single formant. Formant values must be normalized using the log-mean method of Nearey (1978). This may be done using the normalize() function included in this package.}
  \item{vowels}{A vector indicating the vowel category of each vowel in 'formants'. The length of this vector must equal the number of rows in 'formants'.}
}
\details{
This function finds the location of the mean of each vowel category in an n-dimensional, normalized formant-space, where each dimension corresponds to a normalized formant frequency. The pooled, within-category covariance matrix for the formant frequencies is also found. This information is meant to be used in conjunction with the PSTM() function, included in this package. The means and covariance matrix are calculated as described in Nearey & Assmann (2007). The name for each vowel category is stored as the rownames of the 'means' element. These are determined by treating the elements of 'vowels' as a factor and finding the levels of the factor. The function plot() is defined for template objects and allows the user to view the template in a normalized vowel-space.
}
\value{
A 'template' object, a list containing the elements:
\item{means}{A matrix containing the mean location for each vowel category within the normalized formant-space.}
\item{covariance}{The pooled, within-category covariance matrix for the formant frequencies provided.}
}
\references{
Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD thesis, Indiana University Linguistics Club. 


Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding template' models for indirect vowel normalization. in Experimental Approaches to Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala (Oxford University Press, Oxford), pp. 246-269.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

data (pb52)            ## load the Peterson and Barney vowels.
## normalize them.
normdvowels = normalize (pb52[,7:9], pb52$speaker, pb52$vowel)  
formants = normdvowels[,1:3]
vowels = pb52$vowel

## create a vowel template with the normalized formant frequencies
## and information about the vowel category.
template = createtemplate (formants, vowels)

## and inspect with plot()
plot (template)
}







