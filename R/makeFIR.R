makeFIR = function (frequency, power, output = TRUE, verify = FALSE){
  if (!is.numeric(frequency)) stop ('Innappropriate frequency specification.')
  if (!is.numeric(power)) stop ('Innappropriate power specification.')
  if ((frequency[1]) != 0) stop ('First frequency point must be 0.')
  if (length(power) != length (frequency)) stop ('Frequenecy and power specifications of unequal length.')
  if (mean(frequency %/% 2.5 == frequency / 2.5) != 1) stop ('Filter shape points may only be specified at frequencies which are multiples of 2.5.')
  
  power = power - max (power)
  if (min(power) < -96) stop ('Filter power range can not exceed 96 dB.')
 
  n = length (frequency)
  if (n < 2) stop ('At least two points are required to specify the filter.')
  
  freqresponse = interpolate (x = frequency, y = power, increment = 2.5, type = 'linear')[,2]
  
  freqresponse = 10^(freqresponse/20)
  freqresponse = c(freqresponse, rev (freqresponse)[-1])
  y = Re (fft (freqresponse, inverse = TRUE))
  ly = length(y) / 2 + .5
  y = c (rev(y[1:ly]), y[2:ly])
  
  if (verify == TRUE){
    par (mfrow = c(1,2), mar = c(4.5,4.5,3,1))
    spectralslice (y, fs = max(frequency)*2, padding = 20000, main = 'Filter Frequency Response', ylim = c(-100, 10))
    tmp = fft (y)
    plot (atan (Im(tmp) / Re(tmp)), type = 'l', main = 'Filter Phase response', xlab = 'Points', ylab = 'Phase', ylim = c(-1.7, 1.7))
  }
  if (output == TRUE) return (y)
}

