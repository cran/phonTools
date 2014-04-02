# Copyright (c) 2014 Santiago Barreda
# All rights reserved.


errorbars = function(x, y, top, bottom = top, length = .2, add = TRUE, ...){
  if (add) arrows(x, y+top, x, y-bottom, angle=90, code=3, length=length, ...)
  if (!add){
    plot (x,y,pch=16, ylim = range(y) + c(-top, bottom))
    arrows(x, y+top, x, y-bottom, angle=90, code=3, length=length, ...)
  }
}

