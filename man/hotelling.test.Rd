\name{hotelling.test}
\alias{hotelling.test}
\alias{print.hotelling.test}
\title{
Hotelling's T2 Test
}
\description{
Hotelling's T2 test for one and two samples.
}
\usage{
hotelling.test(matrix1, matrix2 = NULL)
}
\arguments{
  \item{matrix1}{a numeric matrix or dataframe in which each column represents observations from a single dimension of a multivariate random variable.}
  \item{matrix2}{an optional second numeric matrix or dataframe of the same column rank as matrix1.}
}
\details{
If a single matrix is provided, this function tests the alternative hypothesis that all column means are not equal to zero. If a second matrix is provided, the alternative hypothesis to be tested is that the group means are not all equal. The statistic is tested using an F-distribution which assumes that the matrices represent multivariate normal variables.

This function is only designed for multivariate tests of location. If a univariate test is desired, please use a t-test.
}
\value{
An object of class 'Hotelling.test', a list containing the elements:

\item{f.value}{the value of the test statistic.}
\item{df1}{the numerator degrees of freedom for the F statistic.}
\item{df2}{the denominator degrees of freedom for the F statistic}
\item{p.value}{the p-value for the test.}
\item{samples}{the number of independent samples involved in the test.}
}
\references{
Hotelling, H. (1931). The generalization of Student's ratio. Annals of Mathematical Statistics 2 (3): 360-378. 

http://en.wikipedia.org/wiki/Hotelling's_T-squared_distribution
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}

\examples{
## load Peterson & Barney data
data (pb52)

## separate the Peterson & Barney vowels by speaker gender and age (child vs. adult)
men = pb52[pb52$sex == 'm' & pb52$type == 'm',]
women = pb52[pb52$sex == 'f' & pb52$type == 'w',]
boys = pb52[pb52$sex == 'm' & pb52$type == 'c',]
girls = pb52[pb52$sex == 'f' & pb52$type == 'c',]

## fit 4 separate models which predict F1 frequency on the basis of vowel category. 
men = rcr (f1 ~ vowel, men$speaker, men)
women = rcr (f1 ~ vowel, women$speaker, women)
boys = rcr (f1 ~ vowel, boys$speaker, boys)
girls = rcr (f1 ~ vowel, girls$speaker, girls)

## A Hotelling T2 test indicates that there are significant differences in F1 frequency 
## based on vowel category between males and females
hotelling.test (men$coefficients, women$coefficients)

## but no significant differences based on the same criteria between boys and girls.
hotelling.test (boys$coefficients, girls$coefficients)
}
