# Copyright (c) 2013 Santiago Barreda
# All rights reserved.

spectralslice = function (sound, padding = length(sound) * 2, fs = 22050,  
    show = TRUE, color = 1, add = FALSE, xlim, ylim, window = "kaiser", windowparameter = 3, 
    zeromax = TRUE, preemphasisf = 0, type, xlab, pwr = FALSE, line = FALSE, ...){

  if (class(sound) == "sound") {
        fs = sound$fs
        sound = sound$sound
  }
  if (preemphasisf > 0) sound = preemphasis (sound, preemphasisf, fs)
  n = length(sound)
  sound = sound * windowfunc(n, window, windowparameter)
  sound = sound - mean(sound)
  N = n + padding
  sound = c(sound, rep(0, padding))
  if ((length(sound)%%2) == 1) sound = c(sound, 0)
  power = abs(fft(sound))^(1 + pwr)
  power = power[1:(N/2+1)]
  power = log(power, 10) * 10
  power[which(power == min(power))] = sort(power)[2]
  if (zeromax == TRUE) power = power - max(power)
  hz = seq(0, fs, length.out = N)[1:length(power)]
  
  if (missing(xlim)) xlim = c(0, fs/2)
  if (missing(ylim)) ylim = range(power[power < xlim[2]])
  if (missing(type)) type = "l"
  if (missing(xlab)) xlab = "Frequency [Hertz]"
  if (add == FALSE & show == TRUE & line == FALSE) 
  plot(hz, power, xlim = xlim, ylim = ylim, ylab = c("Magnitude (dB.)", "Power (dB.)")[(1+pwr)], 
  xlab = xlab, col = color, type = type, xaxs = "i", ...)
  if (add == TRUE & show == TRUE & line == FALSE) lines(hz, power, col = color, type = type, ...)
  if (line == TRUE){
    plot(hz, power, xlim = xlim, ylim = ylim, ylab = c("Magnitude (dB.)", "Power (dB.)")[(1+pwr)], 
    xlab = xlab, col = color, type = 'p', pch = 16, xaxs = "i", ...)
    segments (hz, rep(-500,length(hz)),hz,power)
  }
  dB = power
  invisible(cbind(hz, dB))
}

