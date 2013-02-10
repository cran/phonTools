vowelsynth = function (ffs = c(270, 2200, 2800, 3400, 4400), fbw = 0.06, dur = 300, f0 = c(120,100), 
fs = 10000, verify = FALSE, returnsound = TRUE){
  if (dur < 0) stop ('Positive duration must be specified.')
  if (length (f0) > 2) stop ('Only initial and final f0 may be specified.')
  
  T = 1/fs
  n = round(dur/1000/T)
  if (length(f0) == 1) f0 = c(f0,f0)
  f0 = exp(seq (log(f0[1]), log(f0[2]), length.out = n))
  vsource = NULL
  spot = 1
  while (length (vsource) < n*5){
    tmp = f0[spot]
    cycle = round(fs/tmp) * 2.5
    
    tmp = 2*seq (0,1, 1/cycle) - 3*seq (0,1, 1/cycle)^2 
    tmp = c(rep (0, cycle), tmp)
    vsource = c(vsource, tmp)
    spot = spot + cycle/5
  }
  vsource = FIRfilter(vsource, to = (fs/2)-20, fs = fs*5)
  vsource = vsource[seq(1, n * 5, 5)]
  vsource = jitter(vsource)
  
  x = round (seq (1, n, length.out = 13))
  power = interpolate (x, y = c(0,7.5,9.5,10.5, 11, 11, 11, 11,11,10.5,9.5,7.5,0)/13, increment = 1)[,2]
  power = jitter(power, factor = 0.01)
  vsource = vsource * power
  output = Ffilter (vsource, ffs = ffs, fs = fs, verify = FALSE, bwp = fbw)

  output = output * power
  output = output /(max(abs(output)) * 1.05)
  
  if (verify == TRUE) {
    par (mfrow = c(2,1), mar = c(4,4,1,1))
    plot ((1:n)*T*1000,output, ylab = 'Amplitude', xlab = 'Time (ms)', type = 'l', xaxs = 'i')
    abline (h = 0, lty = 'dotted') 
    spectrogram (output, fs = fs, pause = FALSE, dynamicrange = 60)
  }
  if (returnsound == TRUE) 
    output = makesound(output, "sound.wav", fs = fs)
  return(output)
}
