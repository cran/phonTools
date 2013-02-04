plot.template = function (x, ...){
  sigma = x$covariance[1:2,1:2]
  f1range = range(x$means[,1]) + c(-sigma[1]^.5*2, sigma[1]^.5*2)
  f2range = range(x$means[,2]) + c(-sigma[4]^.5*2, sigma[4]^.5*2)

  plot (x$means[,1:2], type = 'n', xlab = 'F1', ylab = 'F2', xlim = f1range, ylim = f2range)
  text (x$means[,1:2], labels = rownames(x$means))

  for (i in 1:nrow (x$means)){
    t = seq (0,6.3,.1)  
    xs = rbind (cos(t), sin(t))
    A = eigen(sigma)$vectors %*% (diag(sqrt(eigen(sigma)$values)) * 1.96)
    points = t(x$means[i,1:2] + A%*%xs)
    lines (points)
  }
}
