interpolate = function (y, x = 1:length(y), steps = 20, show = FALSE, output = TRUE, ...){

  n = length (y)
  h = x[2:n] - x[1:(n-1)]

  A = matrix (0, n, n)
  A[1,] = c(1, rep (0, n-1))
  A[n,] = c(rep (0, n-1) , 1)
  
  for (i in 2:(n-1)){
    A[i,i-1] = h[i-1]
    A[i,i] = 2*(h[i-1] + h[i])
    A[i,i+1] = h[i]
  }
  
  r = matrix (0, n, 1)
  for (i in 2:(n-1)) r[i] = 6*(((y[i+1] - y[i])/h[i]) - ((y[i] - y[i-1])/h[i-1]))
  
  m = c(0,lm (r ~ A)$coefficients[3:n], 0)
  
  yinterp = NULL; xinterp = NULL;

  for (i in 1:(length(x)-1)){
    a = y[i]
    b = ((y[i+1] - y[i]) / h[i]) - ((h[i]/2)*m[i]) - (h[i]/6)*(m[i+1]-m[i])
    c = m[i]/2
    d = (m[i+1] - m[i]) / (6*h[i])
    
    xx = seq (x[i], x[i+1], (x[i+1]-x[i]) / steps)
    yy = a + b*(xx-x[i]) + c*((xx-x[i])^2) + d*((xx-x[i])^3)
    xinterp = c(xinterp, xx)
    yinterp = c(yinterp, yy)
  }
  if (show == TRUE){     
        plot (xinterp, yinterp, type = 'l')
        points (x, y)
  }  
  if (output == TRUE) return (data.frame (x = xinterp, y = yinterp))
}  