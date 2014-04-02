# Copyright (c) 2014 Santiago Barreda
# All rights reserved.


ldclassify = function (data, means, covariance, posterior = FALSE){
  if (class(means) == 'template'){
    covariance = means$covariance
    means = means$means
  }
  if (ncol (data) != ncol (means)) stop ('Data and means must have the same number of columns.')
  if (ncol (data) != ncol (covariance)) stop ('Innapropriate covariance matrix.')
  if (ncol (covariance) != nrow (covariance)) stop ('Innapropriate covariance matrix.')
  
  data = as.matrix(data)
  means = as.matrix(means)
  covariance = as.matrix(covariance)
  
  distances = sapply (1:nrow(data), function (i){
    tmp = matrix(rep(data[i,], nrow(means)),nrow(means),ncol(means),byrow = TRUE)
    d = diag((tmp-means) %*% solve(covariance)%*% t(tmp-means))
  })
  winner = sapply (1:nrow(data), function (i){
    tmp = order(distances[,i])[1]
  })
  if (!is.null(rownames (means))) winner = as.factor (rownames(means)[winner])
  
  if (posterior){
    posterior = sapply (1:nrow(data), function (i){
      tmp = exp(-sort(distances[,i])[1]/2) / sum (exp(-distances[,i]/2))
    })
    winner = data.frame (winner, posterior)
  }
  return (winner)  
}

