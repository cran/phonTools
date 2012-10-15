sdellipse = function(points, stdev = 1.96, density = .1, output = FALSE, ...){
  if (ncol (points) != 2) stop ('Points input must have exactly two columns.')

  t = seq (0,6.3,density)  
  x = rbind (cos(t), sin(t))
  sigma = var (points)

  A = eigen(sigma)$vectors %*% (diag(sqrt(eigen(sigma)$values)) * stdev)
  points = t(colMeans(points) + A%*%x)
  
  lines (points, ...)
  if (output == TRUE) return (points)
}



