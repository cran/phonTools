# Copyright (c) 2014 Santiago Barreda
# All rights reserved.


ldclassify = function (data, means, covariance){
  if (ncol (data) != ncol (means)) stop ('Data and means must have the same number of columns.')
  if (ncol (data) != ncol (covariance)) stop ('Innapropriate covariance matrix.')
  if (ncol (covariance) != nrow (covariance)) stop ('Innapropriate covariance matrix.')
  
  data = as.matrix(data)
  means = as.matrix(means)
  covariance = as.matrix(covariance)
  
  winners = sapply (1:nrow(data), function (i){
    tmp = matrix(rep(data[i,], nrow(means)),nrow(means),ncol(means),byrow = TRUE)
    d = diag((tmp-means) %*% solve(covariance)%*% t(tmp-means))
    winner = order (d)[1]
  })
  if (!is.null(rownames (means))) winners = as.factor (rownames(means)[winners])
  return (winners)  
}
