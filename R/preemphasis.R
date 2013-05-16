# Copyright (c) 2013 Santiago Barreda
# All rights reserved.

preemphasis = function (input, cutoff = 50, fs = 22050, verify = FALSE, coeff = 0){
  soundout = 0
  if (class(input) == "sound") {
    soundout = 1
    oldsound = input
    fs = input$fs
    input = input$sound
  }
  if (coeff == 0) coeff = -exp (-2 * pi * cutoff / fs)
  out = as.numeric (filter (input, c(1, coeff), method = 'convolution', sides = 1))
  out[1] = input[1]
  if (verify == TRUE){
    spectralslice (out, fs = fs)
    spectralslice (input, fs = fs, add = TRUE, color = 3, lty = 'dotted')    
  }
  if (soundout == 1){
    oldsound$sound = out 
    invisible (oldsound)
  }
  else invisible (out)
}