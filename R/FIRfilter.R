# Copyright (c) 2013 Santiago Barreda
# All rights reserved.

FIRfilter = function (sound, from = 0, to = fs/2, fs = 22050, order = 200, verify = FALSE, repetitions = 1){
  if (repetitions < 1) repetitions = 1
  soundout = 0
  if (class(sound) == "sound") {
    soundout = 1
    tmp = sound
    fs = sound$fs
    sound = sound$sound
  }
  if (order > length (sound)) order = length(sound)
  if (order < 100) order = 100
  if (from < 0) stop ('Low cutoff must be above 0 Hz.')
  if (from > fs/2) stop ('High cutoff must be below the Nyquist frequency.')
 
  spots = rep (c(0,1,3,5,7), repetitions)
  for (i in 1:repetitions){    
  maxamp = max(sound)
  M = order - spots[i]
  n = seq(0,M-1,1)
  Mi = (M-1) / 2  
  fromrads = (((fs/2)-from) / fs) * pi
  torads = (to / fs) * pi
  fromrads2 = (from / fs) * pi
    
  fromh = (-1)^(n)*2*fromrads*sinc((2*fromrads)*(n-Mi))  ##min freq passed
  toh = 2*torads*sinc(2*torads*(n-Mi))  ##max freq passed
  fromh2 = 2*fromrads2*sinc(2*fromrads2*(n-Mi))  
    
  fromh = fromh * windowfunc(length(fromh), type ='blackman')
  toh = toh * windowfunc(length(toh), type ='blackman')
  fromh2 = fromh2 * windowfunc(length(fromh2), type ='blackman')
  
  if (i == 1) output = sound
  if (from!=0 & to==fs/2) output = filter (output, fromh, method = 'convolution', circular = TRUE)
  if (from==0 & to!=fs/2) output = filter (output, toh, method = 'convolution', circular = TRUE)
  if (from!=0 & to!=fs/2) output = filter (output, fromh2-toh, method = 'convolution', circular = TRUE)
  }
  if (verify == TRUE){
    spectralslice (sound, fs = fs, color = 3, lty = 'dashed', ylim = c(-110,0), padding = 0, window = 'kaiser')  
    spectralslice (output, fs = fs, add = TRUE, padding = 0, window = 'kaiser') 
    abline (v = c(from,to), lwd = 2, col = 2)
  }  
  if (soundout == 1){
    tmp$sound = output
    invisible (tmp)
  }
  invisible (output)
}
