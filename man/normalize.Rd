\name{normalize}
\alias{normalize}
\title{
Normalize Vowels
}
\description{
Function to normalize vowels using the log-mean method of Nearey (1978).
}
\usage{
normalize(formants, speakers, vowels)
}
\arguments{
  \item{formants}{A matrix or dataframe containing formant frequency information for vowels. Each row is assumed to indicate data from a single vowel. At least two columns (indicating information regarding at least two formants) are required.}
  \item{speakers}{A vector indicating which speaker produced each vowel in 'formants'. The length of this vector must equal the number of rows in 'formants'.}
  \item{vowels}{A vector indicating the vowel category of each vowel in 'formants'. The length of this vector must equal the number of rows in 'formants'.}
}
\details{
This function uses the log-mean normalization method outlined in Nearey (1978). If Hz values are provided for formant frequencies, they are converted to log Hz values. The mean formant frequency for each vowel category is found for each speaker. The mean is then calculated across all vowel categories. This value represents the average formant frequency produced across all vowels for that speaker. This value is then subtracted from each observed formant frequency vowel for that speaker, effectively centering all formant frequencies in a log space. 

Because the average is found for each vowel category within-speaker before calculating the overall mean, the data from each speaker may contain unequal numbers of each vowel category. However, all speakers must be represented by the same vowel categories or the result will be (possibly) subtle differences in normalized vowel spaces dues to the different estimates of the average formant frequency produced. 

For example, an overall mean calculated from the formant frequencies of /a/ and /e/ will be higher than a mean calculated from only /u/ for the same speaker. This will result in differences in normalized vowel values even though the underlying overall mean formant frequency produced by that speaker is the same.
}
\value{
A list containing the elements:

\item{formants}{A dataframe containing the normalized formant frequencies with the same number of rows and columns as the input data.}
\item{psis}{A vector containing the log-mean of the formant frequencies produced by each speaker. This is the value subtracted from the log Hz of each formant frequency in order to normalize it. Adding this value to a speaker's normalized values and exponentiating the result will 'unnormalize' the vowels. This value should be strongly correlated with speaker vocal-tract length.}
\item{speakers}{A vector containing a label for each speaker as provided in the input. This vector is of the same length as 'psis' and they are presented in the same order.}
}
\references{
Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD thesis, Indiana University Linguistics Club. 
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
## normalize all Peterson & Barney (1952) vowels based
## on the first three formant frequencies.
data (pb52)
normalized = normalize (pb52[,7:8], pb52$speaker, pb52$vowel)

## the benefits of normalizing vowels can be seen by
## using vowelplot() (included in this package)
## the top row of the figure presents unnormalized vowels
## there is substantial overlap between categories
## the bottom row presents normalized vowels
## normalization greatly reduces between-category overlap
par (mfrow = c(2,2), mar = c(3,3,1,1))
vowelplot (pb52$f1, pb52$f2, pb52$vowel, 
           pointType = 16, logaxes = 'xy')
vowelplot (pb52$f1, pb52$f2, pb52$vowel, 
           meansOnly = TRUE, ellipses = TRUE, logaxes = 'xy')

vowelplot (normalized$formants$f1, normalized$formants$f2, 
           pb52$vowel, pointType = 16)
vowelplot (normalized$formants$f1, normalized$formants$f2, 
           pb52$vowel, meansOnly = TRUE, ellipses = TRUE)
}







