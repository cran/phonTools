plot.sound <-
function (x,y, ...){
  time = 1:length(x$sound) / x$fs * 1000
  if (missing (y))y = x$sound
  if (!exists("xlab")) xlab = 'Time (ms)'
  if (!exists("ylab")) ylab = 'Amplitude'
  if (!exists("type")) type = 'l'
  
  touse = seq (1,length(time),2)
  
  plot (time[touse], y[touse], xlab=xlab, ylab=ylab, type, xaxs = 'i', ...)
}
