\name{pickIPA}
\alias{pickIPA}
\alias{ipainfo}
\title{
Pick IPA Symbols
}
\description{
Select IPA symbols that you wish to include in a plot.
}
\usage{
pickIPA (vowels, n = 1, xsampa = FALSE, description = FALSE, vector = FALSE, 
         verify = TRUE)
}
\arguments{
  \item{vowels}{An optional vector of vowel labels that you would like to plot using IPA symbols.} 
  \item{n}{If no vowel vector is provided, the number of symbols desired.} 
  \item{xsampa}{If TRUE, x-sampa versions of the symbols are returned.} 
  \item{description}{If TRUE, description of the place, manner and voicing of the symbols are also returned.} 
  \item{vector}{If TRUE and a vowels vector is provided, the symbols which should be used to represent each element of vowels is returned as a vector. See details.} 
  \item{verify}{If TRUE, the selected symbols are plotted in the order indicated by the user, allowing visual confirmation of the selected symbols.} 
}
\details{
This is an interactive function that allows the user to select IPA symbols for plotting using a chart. The values returned by this function may only be passed to the 'pch' parameter within plotting functions. At the moment it has only been implemented for vowel sounds.

If a vowels vector is given, the function assumes that this vector represents a series of vowel labels which the user wishes to plot. Usually, each member of the vector will correspond to a single observation (i.e. row) from a dataframe. First the number or categories are found, and these are printed out to allow the user to see the number of categories, and the order in which R presents them. After this the user is asked to hit any key to proceed. The user is then asked to click on the desired symbols on the plot. Symbols should be selected in the same order as the categories are printed out in the console. 

The output of the function depends on the setting of the vector parameter. If vector = FALSE, the output will be the same length as the number of vowel categories, and the order of the symbols will correspond to the order of the print out. If vector = TRUE, the output will be the same length as the vowels vector, and each element will correspond to the symbol which should be used to plot each element of the vowels vector.

If a vowels vector is not provided, the function allows the user to select any desired number of symbols, and returns these in the same order indicated by the user. 

**There may be issues when exporting figures to PDF using IPA font. Exporting plots directly as images works 'out of the box'**
}
\value{
A list with the following columns (some of which are optional):

\item{IPA}{The symbol which should be passed to 'pch' to plot IPA symbols.}
\item{xsampa}{The xsampa representation of each IPA character.}
\item{description}{A description of the sound.}
}
\references{
http://en.wikipedia.org/wiki/X-SAMPA
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
##uncomment to run
#vowels = pickIPA (n = 3)
#plot (c(1,2,3), c(1,2,3), pch = vowels)

## select vowels in the order displayed in the console to obtain symbols 
## and descriptions of the vowel categories in the Peterson and Barney data. 
# data (pb52)
# tmp = pickIPA (pb52$vowel, description = TRUE, xsampa = TRUE)
# tmp
}
