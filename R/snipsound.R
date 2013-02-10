snipsound = function (sound, output = TRUE){
  if (class(sound) != "sound")
    stop ('Input must be a sound object')
  time = 1:length(sound$sound)/sound$fs * 1000
  y = sound$sound
  touse = seq(1, length(time), 2)
  plot(time[touse], y[touse], xlab = "Time (ms)", ylab = "Amplitude", type= "l", xaxs = "i")

  times = seq (min(time), max(time), length.out = 1000)
  amps = seq (min(y), max(y), length.out = 10)
  edges = identify(rep(times, length(amps)), rep(amps, length(times)), "", n = 2)
  edges = times[edges]
  edges = sort(edges)

  T = 1/sound$fs
  start = edges[1]/1000/T ; end = edges[2]/1000/T;
  snipped = sound$sound[start:end]
  newtime = T * 1000 * (1:length(snipped))
  plot (newtime[seq(1,length(snipped), 2)], snipped[seq(1,length(snipped), 2)], xlab = "Time (ms)", ylab = "Amplitude", type= "l", xaxs = "i")
 
  if (output == TRUE){
    newsound = makesound (snipped, sound$filename, sound$fs)
    return (newsound)
  }
}

