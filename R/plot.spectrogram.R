plot.spectrogram <-
function (x, y, ylim, pause = TRUE, ...){
  if (x$colors[1] == TRUE)
    zcolors = colorRampPalette (c('blue','cyan','yellow','orange','red', 'brown'))
  if (x$colors[1] == FALSE) zcolors = colorRampPalette (c('white','black'))
  if (length(x$colors) > 1) zcolors = colorRampPalette (x$colors)

  zrange = range(x$spectrogram, finite = TRUE)
  nlevels = abs (zrange[1] - zrange[2]) * 1.2
  
  levels = pretty(zrange, nlevels)
  zcolors = zcolors(length(levels) - 1);
  times = as.numeric(rownames (x$spectrogram))
  hz = as.numeric(colnames (x$spectrogram))

  if (missing (ylim)) ylim = range (0, 5000)
  if (!exists ("xlim")) xlim = range (times)

  plot.new()
  if (pause) readline("  \nResizing figure after plotting may be slow.\n  Please resize plot window now and press <Enter> when ready.\n")
  plot.window(ylim = ylim, xlim = xlim, xaxs = 'i', yaxs = 'i', ...)
  .filled.contour(as.double(times), as.double(hz), x$spectrogram, as.double(levels), 
  col = zcolors)
  box ()
  Axis(times, side = 1)
  Axis(hz, side = 2)
  title(xlab = "Time (ms)", ylab = "Frequenzy (Hz)")
}
