normalize.compare = function (normd){
  for (j in 1:length(normd)){
    tmp = normd[[j]]
    between = 0
    within = 0
    formants = as.matrix (cbind(tmp$f1, tmp$f2))
    vowels = tmp$vowel
    vowelsf = levels(vowels)

    for (i in 1:length(vowelsf)){
      ## FIND RATIO OF BETWEEN-CATEGORY TO WITHIN-CATEGORY SS
      ffs = formants[vowels == vowelsf[i],]
      ffsmean = matrix (colMeans (ffs), nrow(ffs), 2, byrow = TRUE)
      within = within + sum (sqrt (rowSums ((ffs - ffsmean)^2))) / nrow (ffs)
      between = between + sqrt(sum((colMeans (ffs)-colMeans (formants))^2))

    }
   F = between / within
   cat('\n   Method ', j, '\n')
   cat('   Between Category MS / Within Category MS: ', F, '\n')
 }
 cat ('\n')
}

