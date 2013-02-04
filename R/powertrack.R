powertrack = function (sound, timestep = 2, windowlength = timestep, fs = 22050, smoothing = .03, show = TRUE, output = FALSE, ...){
  if (class(sound) == "sound") {
    fs = sound$fs
    sound = sound$sound
  }  
  if (!is.numeric(sound)) stop ('Sound must be numeric.')
  if (timestep < .1) stop ('Timestep must be greater than 0.1 ms.')
  if (windowlength < timestep) stop ('Window length must be greater than or equal to timestep.')
  
  timestep = round (fs * (timestep/1000))
  time = seq (1, length(sound), timestep)
  windowlength = round ((windowlength/1000) * fs / 2)
  
  power = rep (0, length (time))
  sound = c(rep(0, windowlength), sound, rep(0, windowlength))
  for (i in 1:length(time)) power[i] = mean (sound[(time[i]):(time[i]+(windowlength*2))]^2)
  power = power + abs(rnorm(length(power), 0, .00001))
  power = log(power, 10) * 10
  if (smoothing > 0)power = lowess (time, power, f = smoothing)$y
  power = power - max(power)
  
  time = time * (1000/fs)
  tmp = data.frame (time = time, power = power)
  
  if (show == TRUE) plot(tmp$time, tmp$power, xlab = 'Time (ms)', ylab = 'Power (dB)', type = 'l', xaxs = "i", ylim = c(min(power)-1, 2), lwd = 2, col = 4, ...) 
  if (output == TRUE) return (tmp)
}
