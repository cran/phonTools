sinc = function (x, normalized = FALSE){ 
  if (normalized == FALSE) output = sin(x)/x  
  if (normalized == TRUE) output = sin(x*pi)/(x*pi)  
  output[x==0] = 1
  output
}


