makesound = function (sound, filename, fs = 22050){
  if (missing(filename))filename = paste (deparse(substitute(sound)), '.wav', sep='')
  if (!is.numeric(sound)) stop('The sound must be a numeric vector.')

  numSamples = length(sound)
  output = list(filename = filename, fs = fs, numSamples = numSamples, 
  duration = numSamples/fs * 1000, sound = sound)
  class(output) = "sound"
  output
}