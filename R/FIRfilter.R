FIRfilter = function (sound, fir, output = TRUE, verify = FALSE){
  if (class(sound) == "sound") {
    fs = sound$fs
    sound = sound$sound
  }
  if (!is.numeric(sound)) stop ('Non-numeric filter sample values.')
  if (!is.numeric(fir)) stop ('Non-numeric filter impulse response.')

  ly = length(fir) / 2 - .5
  filtered = convolve (sound, rev(fir), type = 'open')[ly:(ly + length(sound) - 1)]
  
  if (verify == TRUE){
    par (mfrow = c(2,2), mar = c(4.5,4.5,3,1))
    spectralslice (sound, main = 'Input Sound', fs = 1, ylim = c(-100,0), padding = 20000, xlab = 'Frequency / Sampling Frequency')
    spectralslice (fir, main='Filter', fs = 1, ylim = c(-100,0), padding = 20000, xlab = 'Frequency / Sampling Frequency')
    spectralslice (filtered, main='Output Sound', fs = 1, ylim = c(-100,0), padding = 20000, xlab = 'Frequency / Sampling Frequency')
    spectralslice (filtered, main='Output + Filter', fs = 1, ylim = c(-100,0), padding = 20000, xlab = 'Frequency / Sampling Frequency')
    spectralslice (fir, lwd = 2, fs = 1, ylim = c(-100,0), padding = 20000, add = TRUE, color = 2)
  }
  if (output == TRUE) return (filtered)
}

