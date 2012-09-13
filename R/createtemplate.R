createtemplate <-
function (formants, vowels){
  means = matrix (0, length(levels(as.factor(vowels))), ncol (formants))
  
  for (i in 1:ncol(formants)) means[,i] = tapply (formants[,i], vowels, mean)
  
  rownames (means) = levels(as.factor(vowels))
  colnames (means) = paste ('f',1:ncol(formants),sep='')
  covariance = var (formants)
  output = list (means = means, covariance = covariance)
  return (output)
}
