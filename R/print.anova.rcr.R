# Copyright (c) 2014 Santiago Barreda
# All rights reserved.

print.anova.rcr <-
function (x, ...){
  cat ("\nSignificance Tests for Groups of Coefficients\n")
  cat ("\nCall:\n")
  print (x$call)
  
  cat ("\n")
  printCoefmat (x$coefficients, has.Pvalue = TRUE)
}
