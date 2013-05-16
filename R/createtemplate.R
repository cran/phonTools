# Copyright (c) 2013 Santiago Barreda
# All rights reserved.

createtemplate <-
function (formants, vowels){
  if (nrow(formants) != length (vowels)) stop ('Formant and vowel dimensions do not match.')
  
  vs = levels(as.factor(vowels))
  nvs = length(vs)
  means = matrix (0, nvs, ncol (formants))
  
  for (i in 1:ncol(formants)) means[,i] = tapply (formants[,i], vowels, mean)
  
  rownames (means) = vs
  colnames (means) = paste ('f',1:ncol(formants),sep='')
  
  tmp = formants
  for (i in 1:nvs) tmp[vowels == vs[i],] = formants[vowels == vs[i],] - matrix (means[i,], nrow(tmp[vowels == vs[i],]), ncol(means), byrow = TRUE)
  covariance  = var(tmp)
  
  output = list (means = means, covariance = covariance)
  class (output) = 'template'
  return (output)
}
