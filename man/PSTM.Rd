\name{PSTM}
\alias{PSTM}
\title{
Probabilistic Sliding Template Model
}
\description{
Classify Vowels Using the Probabilistic Sliding Template Model
}
\usage{
PSTM (x, f0, template, winner = TRUE)
}
\arguments{
  \item{x}{ a matrix of formant frequencies in which each row represents a single vowel token, and each column represents a formant frequency of that vowel. At least 3 formants need to be specified for every vowel.} 
  \item{f0}{ a vector containing the average f0 measured for each vowel to be classified. The length of this vector must equal the number of rows in x.}
  \item{template}{ a 'template' object created with the createtemplate() function provided in this package. If no template is specified, vowels are classified relative to vowels from Edmonton English.}
  \item{winner}{ if TRUE, only the winner of each classification is returned. If FALSE, information regarding all candidate vowels are returned. See 'value' subsection for details.}
}
\details{
The Probabilistic Sliding Template Model (PSTM) is an explicit model of extrinsic vowel normalization. Whereas classic normalization methods, such as Nearey (1978), find a speaker-specific reference parameter based on the speaker's entire vowel system, the PSTM 'guesses' this reference parameter. This function uses 'Method 6' of the PSTM as described in Nearey & Assmann (2007). In this method, the 'guess' is carried out on the basis of the vowel f0, the prior distribution of this parameter across the human population, and the parameter that would be required to make the observed formants a good fit for each candidate vowel category. This method estimates an optimal speaker-specific parameter (equivalent to the log-mean formant frequency produced by the speaker) for each candidate vowel category. The posterior probability that the observed vowel (specified by a row in x) belongs to each candidate category is then determined with reference to the multivariate normal distribution specified by the means and covariance parameters given to the function by the template parameter. 

If no template is passed, the model identifies vowels relative to the vowel system of Edmonton English speakers. For in-depth details regarding the specifics of this model, please see Nearey & Assmann (2007).
}
\value{
If winner = TRUE:

A dataframe with the following columns:

\item{vowel}{ the winning vowel category for each vowel token.}
\item{psi}{ the optimal psi determined for each winning vowel category.}
\item{postprob}{ the posterior probability of the winning vowel category.}

If winner = FALSE:

A list of dataframes, each of which contains information for every candidate vowel category for each token to be classified, where the columns contain the same information as above.
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
formants = normdvowels$formants
vowels = pb52$vowel

## make a vowel template based on these frequencies
template = createtemplate (formants, vowels) 

## first classify only the first three vowels
x = pb52[1:3,c(7:9)] 
f0 = pb52[1:3,6]

## outputting only the winners, and then the full posterior probabilities
PSTM (x, f0, template)
PSTM (x, f0, template, winner = FALSE)

## now classify all vowels
x = pb52[,c(7:9)] 
f0 = pb52[,6]  
winner = PSTM (x, f0, template)
## with a good degree of accuracy
table (winner$vowel, pb52$vowel)
}







