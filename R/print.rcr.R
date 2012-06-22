print.rcr <-
function (x, ...){
  cat ("\nCall:\n")
  print (x$call)
  
  cat ("\nCoefficient Means:\n")
  print (x$coefficient.means)
}
