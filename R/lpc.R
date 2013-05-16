# Copyright (c) 2013 Santiago Barreda
# All rights reserved.

lpc = function (sound, order = round(fs/1000)+3, fs = 10000, show = FALSE, add = FALSE, 
                preemph = TRUE){
  if (class(sound) == "sound") {
    fs = sound$fs
    sound = sound$sound
  }
  
  if (!is.numeric(sound)) stop ('Input must be numeric.')
  if (preemph == TRUE) sound = preemphasis (sound, fs = fs)
  sound = sound - mean (sound)
  sound = sound * windowfunc(length(sound), type = 'hanning')
  tmp = acf(sound, lag.max = length(sound), plot = FALSE)$acf
  
  n = length (sound)
  y = sound[(order+1):n]
  predictors = sapply (seq(1,length(sound)-order, 1), function (x) sound[(x):(x+order-1)])
  predictors = as.matrix(t(predictors))
  mod = lm (y ~ predictors)
  coefficients = as.numeric(rev(mod$coefficients[-1]))
  coefficients = c(1, -coefficients)
  if (show == TRUE & add == TRUE) freqresponse (1, coefficients, fs = fs, add = add)
  if (show == TRUE & add == FALSE){
    freqresponse (1, coefficients, fs = fs, add = add)
    spectralslice (sound, fs = fs, color = 4, add = TRUE)
  }  
  coefficients
}

