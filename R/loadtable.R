# Copyright (c) 2014 Santiago Barreda
# All rights reserved.


loadtable = function (...){
  filename = file.choose()
  read.table (filename, ...)
}


