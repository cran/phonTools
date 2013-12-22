# Copyright (c) 2014 Santiago Barreda
# All rights reserved.

rotate = function (xy, angle, degrees = FALSE){
  complx = FALSE
  if (degrees) angle = angle*pi/180
  if (is.complex(xy)){
    xy = cbind(Re(xy), Im(xy))
    complx = TRUE
  }
  if (ncol(xy) != 2) stop ('Input must have two columns (2-dimensional).')

  rotmat = matrix (c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2,2)
  output = xy %*% rotmat
  if (complx) output = complex (real = output[,1], imaginary = output[,2])
  output
}


