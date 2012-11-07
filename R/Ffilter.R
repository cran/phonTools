Ffilter = function (sound, ffs, bws, fs = 22050, verify = FALSE){
  if (missing(ffs)) stop ('At least one formant center frequency must be provided.')
  if (class(sound) == "sound") {
    fs = sound$fs
    sound = sound$sound
  } 
  if (missing(bws)) bws = rep(0, length(ffs))
  output = sound * 0
  T = 1/fs
  old = sound
  new = old * 0
  for (j in length(ffs):1){
    CF = ffs[j]
    if (bws[j] == 0){
      BW = (CF * .06)
      if (BW < 60)W = 60 
    }
    else BW = bws[j]
    B = -2 * exp (-pi * BW * T) * cos (2 * pi * CF * T)
    C = exp (-2 * pi * BW * T)
    new[1] = old[1]
    new[2] = old[2] - B * new[1]
    for (i in 3:length(old))
      new[i] = old[i] - new[i-1]*B - new[i-2]*C 
    old = new
  }
  if (verify == TRUE){
    par (mfrow = c(2,1))
    spectralslice (sound, fs = fs, ylim = c(-75, 5), pwr = FALSE)
    spectralslice (new, fs = fs, ylim = c(-75, 5), pwr = FALSE)
  }
  return (new)
}




