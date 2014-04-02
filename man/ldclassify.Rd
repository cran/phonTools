\name{ldclassify}
\alias{ldclassify}
\title{
Linear Discriminant Classification
}
\description{
Classify items using linear discriminant analysis. 
}
\usage{
ldclassify (data, means, covariance, posterior = FALSE)
}
\arguments{
  \item{data}{A matrix in which each row represents an item to be classified, and each column represents an observation from a variable describing the item.} 
  \item{means}{A matrix of means where each row is a mean vector specifying a candidate category. The number of columns must equal the number of columns in data. This may be created for a data set using the createtemplate() function included in this package.} 
  \item{covariance}{The pooled within-groups covariance matrix to be used for classification. . This may be created for a data set using the createtemplate() function included in this package.} 
  \item{posterior}{If TRUE, the posterior probability of the winning category is returned.} 
}
\details{
This function classifies the items described in the data matrix by comparing them to the reference patterns for the different candidate categories represented in the means matrix. The category with the minimum Mahalanobis distance to the observed pattern (i.e., a given row of the data matrix) is selected as the winner. Mahalanobis distances are found with using the covariance matrix provided to the function. 
}
\value{
A vector of winning categories is returned. If the means matrix provided had rownames, these are used to indicate winners. If not, the number of the row indicating the winning category in the means matrix is used. If posterior = TRUE, a dataframe is returned that also includes posterior probabilities.
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
## load Peterson & Barney vowel data
data (pb52)

## normalize vowel formant frequencies
normdvowels = normalize (pb52[,7:9], pb52$speaker, pb52$vowel)
formants = normdvowels[,1:3]
vowels = pb52$vowel

## make a vowel template based on these frequencies
template = createtemplate (formants, vowels) 

## classify vowels
answers = ldclassify (formants, template$means, template$covariance)
## compare to known categories
table (answers, vowels)
}

