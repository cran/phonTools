\name{createtemplate}
\alias{createtemplate}
\alias{plot.template}
\title{
Create a Vowel Template
}
\description{
Create a template of vowel-category means and expected variation around these means.
}
\usage{
createtemplate (formants, vowels)
}
\arguments{
  \item{formants}{A matrix of formant frequencies in which each row represents a single token and each column a formant frequency for a single formant.}
  \item{vowels}{A vector indicating the vowel category of each vowel in 'formants'. The length of this vector must equal the number of rows in 'formants'.}
}
\details{
This function finds the location of the mean of each vowel category in an n-dimensional formant-space, where each dimension corresponds to one of n formant frequencies. The pooled, within-category covariance matrix of the formant frequencies is also found. The means and covariance matrix are calculated as described in Nearey & Assmann (2007). The name for each vowel category is stored as the rownames of the 'means' element. These are determined by treating the elements of 'vowels' as a factor and finding the levels of the factor. The function plot() is defined for template objects and allows the user to view the location of and expected variation around the different vowel categories in the formant space.

This information may be used in conjunction with the PSTM() function, included in this package. The mean and covariance matrices provided by this function may also be useful for the ldclassify() function provided in this package. 
}
\value{
A 'template' object, a list containing the elements:
\item{means}{A matrix containing the mean location for each vowel category within the formant-space. Each row represents a different category while each column represents a different formant.}
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

