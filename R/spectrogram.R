# Copyright (c) 2013 Santiago Barreda
# All rights reserved.

spectrogram = function (sound, fs = 22050, windowlength = 5, freqres, timestep = -400, 
    preemphasisf = 50, maxfreq = 5000, colors = TRUE, 
    dynamicrange = 50, nlevels = dynamicrange, maintitle = "", show = TRUE,
    output = FALSE, window = 'kaiser', windowparameter = 3, pause = FALSE){

    if (class(sound) == "sound"){
        fs = sound$fs
        sound = sound$sound
    }
    n = ceiling((fs/1000) * windowlength)     ## n = windowlength
    if (!n%%2) n = n + 1
    if (missing(freqres)) freqres = (fs/n)/4  ## frequency resolution?
    ## determine time step in samples 
    if (timestep > 0) timestep = floor(timestep/1000 * fs)
    if (timestep <= 0) timestep = floor (length(sound) / -timestep)
   if (preemphasisf > 0) sound = preemphasis (sound, preemphasisf, fs)

    sound = c(rep(0, floor(n / 2)), sound, rep(0, floor(n / 2)))
    spots = seq (1, length(sound)-floor(n / 2)*2, timestep)
    padding = (fs - freqres * n)/freqres
    if (padding < 0) padding = 0
    N = n + padding
    if (N%%2) padding = padding + 1

    spect = sapply (spots,function(x){
      tmp = c(sound[x:(x+n-1)], rep(0, padding));
      tmp = tmp * windowfunc(n, window, windowparameter);
      tmp = tmp - mean(tmp);
      tmp = fft (tmp)[1:(N/2+1)];
      tmp = abs(tmp)^2;
      tmp = log(tmp, 10) * 10;
    })
    spect = t(spect)
  
    hz = (0:(N/2)) * (fs/N)
    times = (spots+floor(n / 2)) * (1000/fs)
    rownames(spect) = as.numeric (round(times, 2))
    colnames(spect) = as.numeric (round(hz, 2))

    if (colors == 'alternate') colors = c('black','red','orange','yellow','white')
    if (maxfreq > (fs/2)) maxfreq = fs/2
    spect = spect - max(spect)
    spect[which(spect < (-1 * dynamicrange))] = -1 * dynamicrange
    specobject = list(spectrogram = spect, fs = fs, windowlength = windowlength, 
                 timestep = timestep, dynamicrange = dynamicrange, colors = colors)
    class(specobject) = "spectrogram"

    wait = pause
    if (show == TRUE) plot(specobject, ylim = c(0, maxfreq), pause = wait)

    if (output == TRUE) return(specobject)
} 
