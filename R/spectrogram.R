spectrogram = function (sound, fs = 22050, windowlength = 5, freqres, timestep = 3, 
    preemphasis = 50, maxfreq = 5000, gridlines = FALSE, colors = TRUE, 
    dynamicrange = 40, nlevels = dynamicrange, maintitle = "", show = TRUE,
    output = FALSE, chooseslices = 0, indicateslices = TRUE, zoom = FALSE, 
    indicatezoom = TRUE, window = 'kaiser', windowparameter = 4, pause = TRUE) 
{
    if (class(sound) == "sound") {
        fs = sound$fs
        sound = sound$sound
    }
    if (missing(freqres)) 
        freqres = (fs/ceiling((22050/1000) * windowlength))/4
    n = ceiling((fs/1000) * windowlength)
    first = n + 1
    last = n + length(sound)

    if (preemphasis < 1) preemphasis = 50000
    alpha = exp(-2*pi*preemphasis/fs)
    tmp = sound
    for (i in 2:length(tmp)) tmp[i] = sound[i] - sound[i-1]*alpha
    sound = tmp

    sound = c(rep(0, n * 2), sound, rep(0, n * 2))
    timestep = floor(timestep/1000 * fs)
    padding = (fs - freqres * n)/freqres
    if (padding < 0) 
        padding = 0
    N = n + padding
    if (N%%2) 
        padding = padding + 1
    hz = (0:(N/2)) * (fs/N)
    maxsteps = round((last - first)/timestep)
    spect = matrix(0, maxsteps, (N/2) + 1)
    for (i in 1:maxsteps) {
        current = first + (timestep) * (i - 1) - (n/2)
        snip = sound[current:(current + n - 1)]
        snip = snip * windowfunc (n, window, windowparameter)
        snip = c(snip, rep(0, padding))
        power = abs(fft(snip)^2)
        power = power[1:(N/2 + 1)]
        power = log(power, 10) * 10
        spect[i, ] = power
    }
    times = seq(0, length(sound)/fs, timestep/fs)[1:nrow(spect)] * 
        1000
    rownames(spect) = round(times, 2)
    colnames(spect) = round(hz, 2)
    if (maxfreq > (fs/2)) 
        maxfreq = fs/2
    spect = spect - max(spect)
    spect[which(spect < (-1 * dynamicrange))] = -1 * dynamicrange
    specobject = list(spectrogram = spect, fs = fs, windowlength = windowlength, 
        timestep = timestep, dynamicrange = dynamicrange, colors = colors)
    class(specobject) = "spectrogram"
    wait = pause
    if (show == TRUE) 
        plot(specobject, ylim = c(0, maxfreq), pause = wait)
    if (gridlines == TRUE) 
        abline(h = seq(500, maxfreq, 500), v = seq(25, max(times), 
            25), lty = "dotted", col = "white", lwd = 1.5)
    if (zoom == TRUE) {
        edges = identify(rep(times, length(hz)), rep(hz, length(times)), 
            "", n = 2)
        edges = times[edges%%length(times)]
        edges = sort(edges)
        touse = (times >= edges[1]) & (times <= edges[2])
        zoomspecobject = specobject
        zoomspecobject$spectrogram = spect[touse, ]
        times = times[touse]
        if (indicatezoom == TRUE) 
            abline(v = edges, lwd = 4)
        plot(zoomspecobject, ylim = c(0, maxfreq), pause = wait)
    }
    if (chooseslices > 0) {
        hz = as.numeric(colnames(spect))
        times = as.numeric(rownames(spect))
        slices = identify(rep(times, length(hz)), rep(hz, length(times)), 
            "", n = chooseslices)
        if (length(slices) < chooseslices) 
            stop("Insufficient slices selected.")
        slices = times[slices%%length(times)]
        slices = sort(slices)
        if (indicateslices == TRUE) 
            abline(v = slices, lwd = 4)
        stepsize = 1000/fs
        points = round(slices/stepsize + first)
        slicetimes = round(points * 1000/fs, 3)
        slices = data.frame(slice = 1:chooseslices, time = slicetimes, 
            point = points)
        return(slices)
    }
    if (output == TRUE) 
        return(specobject)
}
