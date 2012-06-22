vowelplot <-
function (f1s, f2s, labels = 0, xrange = NULL, yrange = NULL, meansOnly = FALSE, ellipses = FALSE,ellipsesd = 1.96, add = FALSE, pointType = 0, colors = NULL, logaxes = '', grid = FALSE, gridInterval = c(200,500), gridStart = c(0,0), defaultPlot = TRUE, alternateAxes = FALSE, ...){

  if (is.null(labels)) return (print ('Error: NULL vowel labels given.', quote = FALSE))
  if (is.null(f1s)) return (print ('Error: NULL F1 values given.', quote = FALSE))
  if (is.null(f2s)) return (print ('Error: NULL F2 values given.', quote = FALSE))
  
  labels = as.factor (labels)    ## labels for vowels
  vlevels = levels (labels)      ## levels of vowel categories  
  vnums = as.numeric (labels)    ## a number for each vowel category

  ## specially selected colors to make charts easier to read.
  if (is.null(colors)) colors = colors()[c(24,506,118,610,30,124,556,258,290,151,84,657,404)]
  ##colors = hcl(h = seq(0, 360,by = 360 / length (vlevels)), l = 30, c = 90, alpha = 1)  
 
  # cycles through colors and points if less of these than vowel categories
  if (length (colors) < length (vlevels)) colors = rep (colors, 100)
  if (length (pointType) < length (vlevels)) pointType = rep (pointType, 100)

  ## if ranges arent set, automatically set them at 5% greater/less than vector ranges
  if (is.null(xrange)) xrange = c(range(f1s)[1]*.95, range(f1s)[2]*1.05)
  if (is.null(yrange)) yrange = c(range(f2s)[1]*.95, range(f2s)[2]*1.05)

  if (alternateAxes == TRUE){ temp = f1s; f1s = f2s; f2s = temp;}
  
  # calls initial blank plot function  
  if (add == FALSE & defaultPlot == TRUE & alternateAxes == FALSE) plot (0.1,0.1, type = 'n', xlim = xrange, ylim = yrange, xlab = 'F1', ylab = 'F2', main = '', cex.lab=1.2, cex.axis=1.2, log = logaxes)
  if (add == FALSE & defaultPlot == FALSE & alternateAxes == FALSE) plot (0.1,0.1, type = 'n', log = logaxes, xlim = xrange, ylim = yrange, ...)
  if (add == FALSE & defaultPlot == TRUE & alternateAxes == TRUE) plot (0.1,0.1, type = 'n', xlim = rev(yrange), ylim = rev(xrange), xlab = 'F2', ylab = 'F1', main = '', cex.lab=1.2, cex.axis=1.2, log = logaxes)

  if (grid == TRUE) abline (v = seq (gridStart[1],5000, gridInterval[1]), h = seq (gridStart[2],5000, gridInterval[2]), lwd = .5, lty = 'dotted')
 
  if (meansOnly == FALSE){  ## if individual vowels are to be plotted
    if (labels[1] != 0){ 
      if (pointType[1] == 0) text (f1s, f2s, label = labels, col = colors[vnums])   ## plot text if no pointType specified
      if (pointType[1] != 0) points (f1s, f2s, bg = colors[vnums], col = colors[vnums], pch = pointType[vnums]) ## plot points if so
    }
    if (labels[1] == 0){  
        if (pointType[vnums] == 0) pointType[vnums] = 1  ## if no labels
        points (f1s, f2s, type = 'p', pch = pointType[vnums])
    } 
  }

  if (meansOnly == TRUE){   
    if (labels[1] != 0){  
      f1means = tapply (f1s, labels, mean)
      f2means = tapply (f2s, labels, mean)
      if (pointType[1] == 0) text (f1means, f2means, label = vlevels, col = colors[1:length(vlevels)], cex = 2)   
      if (pointType[1] != 0) points (f1means, f2means, bg = colors[as.numeric(as.factor(labels))], pch = pointType[as.numeric(as.factor(labels))])
    }
    if (labels[1] == 0) return (print ('Error: Mean vowel category plotting only possible if labels are given.', quote = FALSE))
  }

  if (ellipses == TRUE){
    for (i in 1:length(levels(as.factor(labels)))){
      tempf1s = f1s[levels(as.factor(labels))[i] == labels]
      tempf2s = f2s[levels(as.factor(labels))[i] == labels]
      prcs = prcomp (cbind (tempf1s, tempf2s))   ## linear principal components
      
      if (logaxes == 'xy'){
        tempf1s = log(f1s[levels(as.factor(labels))[i] == labels])
        tempf2s = log(f2s[levels(as.factor(labels))[i] == labels])
        prcs = prcomp (cbind (tempf1s, tempf2s))     ## log principal components
      }
      
      xscale = prcs$sdev[1] * ellipsesd 
      yscale = prcs$sdev[2] * ellipsesd 
      rotatedx = mean(tempf1s) + xscale*cos(seq (0,7,.1))*prcs$rotation[1,1] - yscale*sin(seq (0,7,.1))*prcs$rotation[2,1] 
      rotatedy = mean(tempf2s) + xscale*cos(seq (0,7,.1))*prcs$rotation[2,1] + yscale*sin(seq (0,7,.1))*prcs$rotation[1,1]  

      if (logaxes == '')  lines (rotatedx, rotatedy, col = colors[i], lwd = 2)
      if (logaxes == 'xy')  lines (exp(rotatedx), exp(rotatedy), col = colors[i], lwd = 2)
    }
  }
}
