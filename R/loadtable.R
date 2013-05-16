# Copyright (c) 2013 Santiago Barreda
# All rights reserved.

loadtable = function (...){
  filename = file.choose()
  read.table (filename, ...)
}


