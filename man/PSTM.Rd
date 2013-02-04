\name{PSTM}
\alias{PSTM}
\title{
Probabilistic Sliding Template Model
}
\description{
Classify Vowels Using the Probabilistic Sliding Template Model
}
\usage{
PSTM (ffs, f0, template, winner = TRUE)
}
\arguments{
  \item{ffs}{A matrix of formant frequencies in which each row represents a single vowel token, and each column represents a formant frequency. At least 3 formants need to be specified for every vowel.} 
  \item{f0}{A vector containing the average f0 measured for each vowel to be classified. The length of this vector must equal the number of rows in ffs.}
  \item{template}{A 'template' object created with the createtemplate() function provided in this package. If no template is specified, vowels are classified relative to vowels from Edmonton English.}
  \item{winner}{If TRUE, only the winner of each classification is returned. If FALSE, information regarding all candidate vowels is returned. See 'value' subsection for details.}
}
\details{
The Probabilistic Sliding Template Model (PSTM) is an explicit model of vowel normalization. The classic log-mean normalization method of Nearey (1978), finds a speaker-specific reference parameter based on the speaker's entire vowel system. This parameter is related to the range of formant frequencies that a speaker can be expected to produce. The PSTM works by normalizing the formant frequencies of a vowel based on a speaker-specific fisplacement parameter (psi) and then classifying the vowel relative to a normalized vowel space. The 'template' object given to this vowel indicates the average formant frequencies expected for each vowel category, as well information regarding the covariances of these distributions. Using this information, a normalized vowel can be classified by selecting the category with the highest likelihood given the expected mean for that vowel category (Nearey & Assmann, 2007).

'Method 6' of the PSTM (described in Nearey & Assmann, 2007) 'guesses' the speaker-specific reference parameter (psi) for each vowel token. The method guesses a different psi for each candidate vowel category using information regarindg vowel f0, the prior distribution of psi across the human population, and the psi that would be required to make the observed formants a good fit for the candidate vowel category. Each vowel vowel is normalized by its specific psi, and the posterior probability of observing the vowel given the formants is found (based on the information provided in the 'template' object). 

If no template is passed, the model identifies vowels relative to the vowel system of Edmonton English speakers. For in-depth details regarding the specifics of this model, please see Nearey & Assmann (2007).
}
\value{
If winner = TRUE:

A dataframe with the following columns:

\item{vowel}{The label for each winning vowel.}
\item{psi}{The optimal psi determined for the winning vowel.}
\item{postprob}{The posterior probability of observing the winning vowel, given the formants and the psi.}

If winner = FALSE:

A list of dataframes, each of which contains information for every candidate vowel category for each token to be classified. Each dataframe has the following columns:

\item{vowel}{The label for each candidate vowel category.}
\item{psi}{The optimal psi determined for each vowel category.}
\item{postprob}{The posterior probability of observing each vowel category, given the formants and the psi.}

}
\references{
Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD thesis, Indiana University Linguistics Club. 


Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding template' models for indirect vowel normalization. in Experimental Approaches to Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala (Oxford University Press, Oxford), pp. 246-269.
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

## first classify only the first three vowels
ffs = pb52[1:3,c(7:9)] 
f0 = pb52[1:3,6]

## outputting only the winners, and then the full posterior probabilities
PSTM (ffs, f0, template)
PSTM (ffs, f0, template, winner = FALSE)

## now classify all vowels
ffs = pb52[,c(7:9)] 
f0 = pb52[,6]  
winner = PSTM (ffs, f0, template)
## with a good degree of accuracy
table (winner$vowel, pb52$vowel)

}







