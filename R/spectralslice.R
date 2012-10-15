spectralslice = function (sound, padding = length(sound) * 2, fs = 22050, output = FALSE, 
    show = TRUE, color = 1, add = FALSE, xlim, ylim, window = "kaiser", 
    windowparameter = 4, zeromax = TRUE, preemphasis = 50000, type, xlab, ...){

  if (preemphasis < 1) preemphasis = 50000
  alpha = exp(-2 * pi * preemphasis/fs)
  tmp = sound
  for (i in 2:length(tmp)) tmp[i] = sound[i] - sound[i - 1] * alpha
  sound = tmp
  n = length(sound)
  sound = sound * windowfunc(n, window, windowparameter)
  N = n + padding
  sound = c(sound, rep(0, padding))
  if ((length(sound)%%2) == 1) sound = c(sound, 0)
  power = abs(fft(sound)^2)
  power = power[1:(N/2 + 1)]
  power = log(power, 10) * 10
  power[which(power == min(power))] = sort(power)[2]
  if (zeromax == TRUE) power = power - max(power)
  hz = (0:(N/2)) * (fs/N)
  
  if (missing(xlim)) xlim = c(0, fs/2)
  if (missing(ylim)) ylim = range(power[power < xlim[2]])
  if (missing(type)) type = "l"
  if (missing(xlab)) xlab = "Frequency [Hertz]"
  if (add == FALSE & show == TRUE) 
  plot(hz, power, xlim = xlim, ylim = ylim, ylab = "Magnitude (dB.)", 
  xlab = xlab, col = color, type = type, xaxs = "i", ...)
  if (add == TRUE & show == TRUE) lines(hz, power, col = color, type = type, ...)
  if (output) return(cbind(hz, power))
}