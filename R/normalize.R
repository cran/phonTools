normalize <-
function (formants, speakers, vowels){
  if (is.null(ncol(formants))) return (cat("Error: must provide at least two formants (i.e. F1, F2, ...)"))
  if (length(speakers) != nrow (formants)) return (cat('Error: Speaker vector length does not match formant data length.'))
  if (length(vowels) != nrow (formants)) return (cat('Error: Formant vector length does not match formant data length.'))
 
 speakers = as.character (speakers) 
 vowels = as.character (vowels) 

 speakersf = levels (as.factor (speakers))
 if (max(formants) > 30)formants = log (formants)
 fs = rowSums (formants) / ncol (formants)

 psis = NULL

 for (i in 1:length (speakersf)){
   temp = (speakers == speakersf[i])
   psi = mean (tapply (fs[temp], data.frame (speakers[temp], vowels[temp]), mean))
   psis = c(psis, psi)
   formants[temp,] = formants[temp,] - psi
 }

 output = list (formants = formants, psis = psis, speakers = speakersf)
 return (output)
}
