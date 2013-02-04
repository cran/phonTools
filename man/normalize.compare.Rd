\name{normalize.compare}
\alias{normalize.compare}
\title{
Compare Normalization Methods
}
\description{
Compare the effectiveness of different normalization methods. 
}
\usage{
normalize.compare (normd)
}
\arguments{
  \item{normd}{ A list of dataframes containing the different formant data to be compared. Each dataframe must contain columns called "f1", "f2", "speaker", and "vowel", in any order.} 
}
\details{
This function provides a relatively-straightforward way to compare the effectiveness of different normalization methods based on two assumptions:

1) Normalized vowels with the same phonetic content should be as close as possible in the normalized space, so within-category variation should be minimized. 

2) Normalized vowels with differing phonetic contents should be as separated as possible in the normalized space, so between-category variation should be maximized. 

For each dataframe provided, the function calculates the ratio of between-category variation over within-category variation. Variation is calculated based on euclidean distances in a two-dimensional space. 

For example, for each vowel category, the average distance of each token to the category mean is found. This is then compared to the distance between the mean location for that vowel category to the grand mean for the entire vowel system. The resulting value is analogous to the F statistic provided by an Analysis of Variance in which vowel categories are trated as factors. 
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{

## load the Peterson and Barney vowels
data (pb52)

## normalize using several different methods
neareyE = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, method = 'neareyE')
neareyI = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, method = 'neareyI')
lobanov = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, method = 'lobanov')
wandf = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, method = 'wandf', corners =  c('iy','aa'))

## compare outcome of methods (and unnormalized vowels)
normd = list (pb52, neareyE, neareyI, lobanov, wandf)
normalize.compare (normd)

## compare normalization methods using vowelplot().
par (mfrow = c(2,2), mar = c(4,4,3,1))
vowelplot (neareyE[,1], neareyE[,2], neareyE$vowel, alternateAxes = TRUE, 
  pointType = 16, main = 'neareyE', ellipses = TRUE)
vowelplot (neareyI[,1], neareyI[,2], neareyI$vowel, alternateAxes = TRUE, 
  pointType = 16, main = 'neareyI', ellipses = TRUE)
vowelplot (lobanov[,1], lobanov[,2], lobanov$vowel, alternateAxes = TRUE, 
  pointType = 16, main = 'lobanov', ellipses = TRUE)
vowelplot (wandf[,1], wandf[,2], wandf$vowel, alternateAxes = TRUE, 
  pointType = 16, main = 'wandf', ellipses = TRUE)

}





