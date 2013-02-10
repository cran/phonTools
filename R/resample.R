resample = function (sound, newfs, oldfs){
  soundout = 0
  if (class(sound) == "sound") {
    soundout = 1
    oldsound = sound
    oldfs = sound$fs
    sound = sound$sound
  }
  if (oldfs > newfs) if (oldfs %% newfs != 0) stop ('Old sampling frequency must be an integer multiple of new sampling frequency.')
  if (oldfs < newfs) if (newfs %% oldfs != 0) stop ('New sampling frequency must be an integer multiple of old sampling frequency.')
 
  if (oldfs > newfs){
    ratio = oldfs / newfs
    sound = FIRfilter (sound, to = ((oldfs/2)/ratio)-20, fs = oldfs, order = 2000)
    sound = sound[seq(1, length(sound), ratio)]
  }

  if (oldfs < newfs){
    ratio = newfs / oldfs    
    tmp = NULL
    for (i in 1:length (sound)){
      tmp = c(tmp, sound[i])
      tmp = c(tmp, rep (0, ratio-1))
    }
    sound = FIRfilter (tmp, to = ((oldfs/2))-20, fs = newfs, order = 2000)
  }
  sound = sound / (max(sound) * 1.05) 
  if (soundout == 1)  sound = makesound (sound, filename = oldsound$filename, fs = newfs)
  return (sound)   
}
