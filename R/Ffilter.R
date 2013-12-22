# Copyright (c) 2014 Santiago Barreda
# All rights reserved.


Ffilter = function (sound, ffs, bwp = 0.06, fs = 22050, verify = FALSE) 
{
    if (missing(ffs)) 
        stop("At least one formant center frequency must be provided.")
    if (class(sound) == "sound") {
        fs = sound$fs
        sound = sound$sound
    }
    if (is.numeric(ffs)) 
        ffs = list(ffs, ffs)
    nffs = length(ffs[[1]])
    if (length(ffs) != 2) 
        stop("Only initial and final formant values may be provided.")
    if (length(ffs[[1]]) != length(ffs[[2]])) 
        stop("Same number of formants must be provided for beginning and\n end points.")
    tmp = matrix(0, length(sound), nffs)
    for (i in 1:nffs) tmp[, i] = seq(ffs[[1]][i], ffs[[2]][i], 
        length.out = length(sound))
    ffs = tmp
    if (length(bwp) == 1) 
        bwp = rep(bwp, nffs)
    if (length(bwp) != nffs) 
        stop("Number of bandwidths specificed must equal number of formants.")
    percent = TRUE
    if (max(bwp) > 1) 
        percent = FALSE
    output = sound * 0
    T = 1/fs
    old = sound
    new = old * 0
    for (j in nffs:1) {
        CF = ffs[, j]
        if (percent) 
            BW = (CF * bwp[j])
        if (!percent) 
            BW = rep(bwp[j], length(sound))
        C = -exp(-2 * pi * BW * T)
        B = 2 * exp(-pi * BW * T) * cos(2 * pi * CF * T)
        A = 1 - B - C
        new[1] = old[1] * A[1]
        new[2] = old[2] * A[2] - B[2] * new[1]
        for (i in 3:length(old)) new[i] = old[i] * A[i] + new[i - 
            1] * B[i] * sqrt(A[i]/A[i - 1]) + new[i - 2] * C[i] * 
            sqrt(A[i]/A[i - 1])
        old = new
    }
    if (verify == TRUE) {
        par(mfrow = c(2, 1))
        spectralslice(sound, fs = fs, ylim = c(-75, 5))
        spectralslice(new, fs = fs, ylim = c(-75, 5))
    }
    invisible(new)
}
