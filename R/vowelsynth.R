vowelsynth = function (ffs = c(270, 2200, 2800, 3400), bws, dur = 200, f0 = 120, 
    fs = 10000, verify = FALSE, returnsound = TRUE) 
{
    if (max(ffs) > fs/2) 
        stop("Formant frequencies must be below the nyquist frequency.")
    if (missing(bws)) 
        bws = rep(0, length(ffs))
    T = 1/fs
    n = round(dur/1000/T)
    knots = round(n/15)
    power = interpolate(y = c(-50, 0, -1, -50), x = c(1, knots, 
        n - knots * 2, n), increment = 1, type = "linear")[,2]
    power = 10^(power/20)
    cycle = round(fs/f0)
    ptrain = NULL
    flutter = 0
    for (i in 1:(n/cycle + 20)) {
        flutter = sample(c(-1, 1, 0, 0, 0), 1)
        ptrain = c(ptrain, rep(1, 1), rep(0, cycle + flutter))
    }
    vsource = Ffilter(ptrain, ffs = 0, bws = 100, fs = fs)
    vsource = rev(vsource)[1:n]
    vsource = c(0, vsource[2:n] - vsource[1:(n - 1)])
    vsource = (vsource - mean(vsource))/max(vsource)
    vsource = vsource + rnorm(length(vsource), 0, 0.05)
    vsource = vsource * power
    output = Ffilter(vsource, ffs = ffs, bws = bws, fs = 10000, 
        verify = verify)
    output = output * power
    output = (output - mean(output))/(max(output) * 1.1)

    if (returnsound == TRUE) output = makesound (output, 'sound.wav', fs = fs)
    return(output)
}
