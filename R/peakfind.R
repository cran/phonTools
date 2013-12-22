# Copyright (c) 2014 Santiago Barreda
# All rights reserved.


peakfind = function (x, show = TRUE){
  rightbig = c(0, (apply(matrix(x), 2, FUN = diff) > 0)^2)
  leftbig = c((-1*apply(matrix(x), 2, FUN = diff) > 0)^2, 0)
  peaks = leftbig + rightbig
  npeaks = sum (peaks == 2)
  peaks = order (peaks, decreasing = TRUE)[1:npeaks]
  
  if (show == TRUE){
    plot (x, lwd = 2, type = 'l', ylab = 'Value', xaxs = 'i')
    points (peaks, x[peaks], col = 2, pch = 17, cex = 1)
  }
  invisible (peaks)
}
